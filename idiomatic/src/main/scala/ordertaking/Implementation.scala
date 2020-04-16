package ordertaking

import ordertaking._
import PublicTypes._

import zio.IO

object Implementation {

  sealed trait AddressValidationError
  case object InvalidFormat extends AddressValidationError
  case object AddressNotFound extends AddressValidationError

  case class CheckedAddress(address: UnvalidatedAddress)

  case class ValidatedOrderLine(
    orderLineId: OrderLineId,
    productCode: ProductCode,
    quantity: OrderQuantity
  )

  case class ValidatedOrder(
    orderId: OrderId,
    customerInfo: CustomerInfo,
    shippingAddress: Address,
    billingAddress: Address,
    lines: List[ValidatedOrderLine]
  )

  type ValidateOrder =
    (ProductCode => Boolean) // dependency
    => (UnvalidatedAddress => IO[AddressValidationError, CheckedAddress]) // dependency
    => UnvalidatedOrder // input
    => IO[ValidationError, ValidatedOrder] // output

  case class HtmlString(value: String) extends AnyVal

  case class OrderAcknowledgment(
    emailAddress: EmailAddress,
    letter: HtmlString
  )

// Send the order acknowledgement to the customer
// Note that this does NOT generate an Result-type error (at least not in this workflow)
// because on failure we will continue anyway.
// On success, we will generate a OrderAcknowledgmentSent event,
// but on failure we won't.

  sealed trait SendResult
  case object Sent extends SendResult
  case object NotSent extends SendResult

// ======================================================
// Section 2 : Implementation
// ======================================================

  val toCustomerInfo: UnvalidatedCustomerInfo => Result[ValidationError, CustomerInfo] =
    unvalidatedCustomerInfo =>
      for {
        firstName <- String50
          .create("FirstName", unvalidatedCustomerInfo.firstName)
          .mapError(ValidationError(_))
        lastName <- String50
          .create("LastName", unvalidatedCustomerInfo.lastName)
          .mapError(ValidationError(_))
        emailAddress <- EmailAddress
          .create("EmailAddress", unvalidatedCustomerInfo.emailAddress)
          .mapError(ValidationError(_))
      } yield CustomerInfo(
        PersonalName(firstName, lastName),
        emailAddress
      )

// ---------------------------
// ValidateOrder step
// ---------------------------

  val toAddress: CheckedAddress => Result[ValidationError, Address] =
    checkedAddress => {
      val address = checkedAddress.address
      for {
        addressLine1 <- String50.create("AddressLine1", address.addressLine1).mapError(ValidationError(_))
        addressLine2 <- String50.createOption("AddressLine1", address.addressLine2).mapError(ValidationError(_))
        addressLine3 <- String50.createOption("AddressLine1", address.addressLine3).mapError(ValidationError(_))
        addressLine4 <- String50.createOption("AddressLine1", address.addressLine4).mapError(ValidationError(_))
        city <- String50.create("City", address.city).mapError(ValidationError(_))
        zipCode <- ZipCode.create("ZipCode", address.zipCode).mapError(ValidationError(_))
      } yield Address(
        addressLine1,
        addressLine2,
        addressLine3,
        addressLine4,
        city,
        zipCode
      )
    }

  // Call the checkAddressExists and convert the error to a ValidationError
  def toCheckedAddress(
    checkAddress: UnvalidatedAddress => IO[AddressValidationError, CheckedAddress],
    address: UnvalidatedAddress
  ): IO[ValidationError, CheckedAddress] =
    checkAddress(address).mapError {
      case AddressNotFound => ValidationError("Address not found")
      case InvalidFormat   => ValidationError("Address has bad format")
    }

  val toOrderId: String => Result[ValidationError, OrderId] =
    orderId =>
      OrderId
        .create("OrderId", orderId)
        .mapError(ValidationError(_)) // convert creation error into ValidationError

// Helper function for validateOrder
  val toOrderLineId: String => Result[ValidationError, OrderLineId] =
    orderLineId =>
      OrderLineId
        .create("OrderLineId", orderLineId)
        .mapError(ValidationError(_)) // convert creation error into ValidationError

  // Helper function for validateOrder
  def toProductCode(
    checkProductCodeExists: ProductCode => Boolean,
    productCode: String
  ): Result[ValidationError, ProductCode] = {
    // create a ProductCode -> Result<ProductCode,...> function
    // suitable for using in a pipeline
    val checkProduct = (productCode: ProductCode) =>
      if (checkProductCodeExists(productCode))
        Ok(productCode)
      else
        Error(ValidationError(s"Invalid: $productCode"))

    // assemble the pipeline
    ProductCode
      .create("ProductCode", productCode)
      .mapError(ValidationError) // convert creation error into ValidationError
      .flatMap(checkProduct)
  }

  // Helper function for validateOrder
  def toOrderQuantity(productCode: ProductCode, quantity: BigDecimal): Result[ValidationError, OrderQuantity] =
    OrderQuantity
      .create("OrderQuantity", productCode, quantity)
      .mapError(ValidationError(_)) // convert creation error into ValidationError

// Helper function for validateOrder
  def toValidatedOrderLine(
    checkProductCodeExists: ProductCode => Boolean,
    unvalidatedOrderLine: UnvalidatedOrderLine
  ): Result[ValidationError, ValidatedOrderLine] =
    for {
      orderLineId <- toOrderLineId(unvalidatedOrderLine.orderLineId)
      productCode <- toProductCode(checkProductCodeExists, unvalidatedOrderLine.productCode)
      quantity <- toOrderQuantity(productCode, unvalidatedOrderLine.quantity)
    } yield ValidatedOrderLine(orderLineId, productCode, quantity)

  val validateOrder: ValidateOrder =
    checkProductCodeExists =>
      checkAddressExists =>
        unvalidatedOrder =>
          for {
            orderId <- toOrderId(unvalidatedOrder.orderId).toAsyncResult
            customerInfo <- toCustomerInfo(unvalidatedOrder.customerInfo).toAsyncResult
            checkedShippingAddress <- toCheckedAddress(checkAddressExists, unvalidatedOrder.shippingAddress)
            shippingAddress <- toAddress(checkedShippingAddress).toAsyncResult
            checkedBillingAddress <- toCheckedAddress(checkAddressExists, unvalidatedOrder.billingAddress)
            billingAddress <- toAddress(checkedBillingAddress).toAsyncResult
            lines <- Result
              .sequence(unvalidatedOrder.lines.map(line => toValidatedOrderLine(checkProductCodeExists, line)))
              .toAsyncResult
          } yield ValidatedOrder(
            orderId,
            customerInfo,
            shippingAddress,
            billingAddress,
            lines
          )

// ---------------------------
// PriceOrder step
// ---------------------------

  def toPricedOrderLine(
    getProductPrice: ProductCode => Price,
    validatedOrderLine: ValidatedOrderLine
  ): Result[PricingError, PricedOrderLine] = {
    val qty = validatedOrderLine.quantity.value
    val price = getProductPrice(validatedOrderLine.productCode)
    Price
      .multiply(qty, price)
      .mapError(PricingError(_))
      .map { linePrice =>
        PricedOrderLine(
          validatedOrderLine.orderLineId,
          validatedOrderLine.productCode,
          validatedOrderLine.quantity,
          linePrice
        )
      }
  }

  def priceOrder(
    getProductPrice: ProductCode => Price,
    validatedOrder: ValidatedOrder
  ): Result[PricingError, PricedOrder] =
    for {
      lines <- Result.sequence(validatedOrder.lines.map(toPricedOrderLine(getProductPrice, _)))
      amountToBill <- BillingAmount
        .sumPrices(lines.map(_.linePrice)) // get each line price & add them together as a BillingAmount
        .mapError(PricingError(_)) // convert to PlaceOrderError
    } yield PricedOrder(
      validatedOrder.orderId,
      validatedOrder.customerInfo,
      validatedOrder.shippingAddress,
      validatedOrder.billingAddress,
      amountToBill,
      lines
    )

// ---------------------------
// AcknowledgeOrder step
// ---------------------------

  def acknowledgeOrder(
    createAcknowledgmentLetter: PricedOrder => HtmlString,
    sendAcknowledgment: OrderAcknowledgment => SendResult,
    pricedOrder: PricedOrder
  ): Option[OrderAcknowledgmentSent] = {
    val letter = createAcknowledgmentLetter(pricedOrder)
    val acknowledgment = OrderAcknowledgment(
      emailAddress = pricedOrder.customerInfo.emailAddress,
      letter = letter
    )

    // if the acknowledgement was successfully sent,
    // return the corresponding event, else return None
    sendAcknowledgment(acknowledgment) match {
      case Sent =>
        val event = OrderAcknowledgmentSent(
          orderId = pricedOrder.orderId,
          emailAddress = acknowledgment.emailAddress
        )
        Some(event)
      case NotSent =>
        None
    }
  }

// ---------------------------
// Create events
// ---------------------------

  val createOrderPlacedEvent: PricedOrder => OrderPlaced = placedOrder => OrderPlaced(placedOrder)

  val createBillingEvent: PricedOrder => Option[BillableOrderPlaced] =
    placedOrder => {
      val billingAmount = placedOrder.amountToBill.value
      if (billingAmount > BigDecimal(0.0))
        Some(
          BillableOrderPlaced(
            placedOrder.orderId,
            placedOrder.billingAddress,
            placedOrder.amountToBill
          )
        )
      else
        None
    }

  def createEvents(
    pricedOrder: PricedOrder,
    acknowledgmentEventOpt: Option[OrderAcknowledgmentSent]
  ): List[PlaceOrderEvent] = {
    val acknowledgmentEvents =
      acknowledgmentEventOpt.toList
    val orderPlacedEvents =
      List(createOrderPlacedEvent(pricedOrder))
    val billingEvents =
      createBillingEvent(pricedOrder).toList

    acknowledgmentEvents ++ orderPlacedEvents ++ billingEvents
  }

  // ---------------------------
// overall workflow
// ---------------------------

  def placeOrder(
    checkProductExists: ProductCode => Boolean,
    checkAddressExists: UnvalidatedAddress => IO[AddressValidationError, CheckedAddress],
    getProductPrice: ProductCode => Price,
    createOrderAcknowledgmentLetter: PricedOrder => HtmlString,
    sendOrderAcknowledgment: OrderAcknowledgment => SendResult,
    unvalidatedOrder: UnvalidatedOrder
  ): IO[PlaceOrderError, List[PlaceOrderEvent]] =
    for {
      validatedOrder <- validateOrder(checkProductExists)(checkAddressExists)(unvalidatedOrder)
      pricedOrder <- priceOrder(getProductPrice, validatedOrder).toAsyncResult
      acknowledgementOption = acknowledgeOrder(
        createOrderAcknowledgmentLetter,
        sendOrderAcknowledgment,
        pricedOrder
      )
    } yield createEvents(pricedOrder, acknowledgementOption)

}
