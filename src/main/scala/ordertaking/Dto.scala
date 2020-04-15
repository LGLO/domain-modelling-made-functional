package ordertaking

import ordertaking._
import PublicTypes._

object Dto {

  case class AddressDto(
    addressLine1: String,
    addressLine2: String,
    addressLine3: String,
    addressLine4: String,
    city: String,
    zipCode: String
  ) {

    def toUnvalidatedAddress = UnvalidatedAddress(
      addressLine1,
      addressLine2,
      addressLine3,
      addressLine4,
      city,
      zipCode
    )

    /// Convert the DTO into a Address object
    /// Used when importing from the outside world into the domain, eg loading from a database.
    def toAddress: Result[String, Address] =
      for {
        addressLine1 <- String50.create(addressLine1, "addressLine1")
        addressLine2 <- String50.createOption(addressLine2, "addressLine2")
        addressLine3 <- String50.createOption(addressLine3, "addressLine3")
        addressLine4 <- String50.createOption(addressLine4, "addressLine4")
        city <- String50.create(city, "city")
        zipCode <- ZipCode.create(zipCode, "zipCode")
      } yield Address(
        addressLine1,
        addressLine2,
        addressLine3,
        addressLine4,
        city,
        zipCode
      )
  }

  object AddressDto {

    /// Convert a Address object into the corresponding DTO.
    /// Used when exporting from the domain to the outside world.
    // this is a simple 1:1 copy
    val fromAddress: Address => AddressDto =
      domainObj =>
        AddressDto(
          domainObj.addressLine1.value,
          domainObj.addressLine2.map(_.value).getOrElse(null),
          domainObj.addressLine3.map(_.value).getOrElse(null),
          domainObj.addressLine4.map(_.value).getOrElse(null),
          domainObj.city.value,
          domainObj.zipCode.value
        )
  }

//===============================================
// DTO for CustomerInfo
//===============================================

  case class CustomerInfoDto(
    firstName: String,
    lastName: String,
    emailAddress: String
  ) {

    /// Convert the DTO into a UnvalidatedCustomerInfo object.
    /// This always succeeds because there is no validation.
    /// Used when importing an OrderForm from the outside world into the domain.
    def toUnvalidatedCustomerInfo: UnvalidatedCustomerInfo =
      // sometimes it's helpful to use an explicit type annotation
      // to avoid ambiguity between records with the same field names.
      UnvalidatedCustomerInfo(
        // this is a simple 1:1 copy which always succeeds
        firstName = firstName,
        lastName = lastName,
        emailAddress = emailAddress
      )

    /// Convert the DTO into a CustomerInfo object
    /// Used when importing from the outside world into the domain, eg loading from a database
    def toCustomerInfo: Result[String, CustomerInfo] =
      for {
        // get each (validated) simple type from the DTO as a success or failure
        first <- String50.create("FirstName", firstName)
        last <- String50.create("LastName", lastName)
        email <- EmailAddress.create("EmailAddress", emailAddress)
        // combine the components to create the domain object
        name = PersonalName(firstName = first, lastName = last)
      } yield CustomerInfo(name = name, emailAddress = email)
  }

/// Functions for converting between the DTO and corresponding domain object
  object CustomerInfoDto {

    /// Convert a CustomerInfo object into the corresponding DTO.
    /// Used when exporting from the domain to the outside world.
    val fromCustomerInfo: CustomerInfo => CustomerInfoDto =
      domainObj =>
        CustomerInfoDto(
          firstName = domainObj.name.firstName.value,
          lastName = domainObj.name.lastName.value,
          emailAddress = domainObj.emailAddress.value
        )
  }

//===============================================
// DTOs for OrderLines
//===============================================

/// From the order form used as input
  case class OrderFormLineDto(
    orderLineId: String,
    productCode: String,
    quantity: BigDecimal
  ) {

    /// Convert the OrderFormLine into a UnvalidatedOrderLine
    /// This always succeeds because there is no validation.
    /// Used when importing an OrderForm from the outside world into the domain.
    // this is a simple 1:1 copy
    def toUnvalidatedOrderLine: UnvalidatedOrderLine =
      UnvalidatedOrderLine(orderLineId = orderLineId, productCode = productCode, quantity = quantity)
  }

//===============================================
// DTOs for PricedOrderLines
//===============================================

/// Used in the output of the workflow
  case class PricedOrderLineDto(
    orderLineId: String,
    productCode: String,
    quantity: BigDecimal,
    linePrice: BigDecimal
  )

  object PricedOrderLineDto {
    /// Convert a PricedOrderLine object into the corresponding DTO.
    /// Used when exporting from the domain to the outside world.
    // this is a simple 1:1 copy

    val fromDomain: PricedOrderLine => PricedOrderLineDto =
      domainObj =>
        PricedOrderLineDto(
          orderLineId = domainObj.orderLineId.value,
          productCode = domainObj.productCode.value,
          quantity = domainObj.quantity.value,
          linePrice = domainObj.linePrice.value
        )
  }

//===============================================
// DTO for OrderForm
//===============================================

  case class OrderFormDto(
    orderId: String,
    customerInfo: CustomerInfoDto,
    shippingAddress: AddressDto,
    billingAddress: AddressDto,
    lines: List[OrderFormLineDto]
  ) {

/// Convert the OrderForm into a UnvalidatedOrder
    /// This always succeeds because there is no validation.
    def toUnvalidatedOrder: UnvalidatedOrder =
      UnvalidatedOrder(
        orderId = orderId,
        customerInfo = customerInfo.toUnvalidatedCustomerInfo,
        shippingAddress = shippingAddress.toUnvalidatedAddress,
        billingAddress = billingAddress.toUnvalidatedAddress,
        lines = lines.map(_.toUnvalidatedOrderLine)
      )

  }

//===============================================
// DTO for OrderPlaced event
//===============================================

/// Event to send to shipping context
  case class OrderPlacedDto(
    orderId: String,
    customerInfo: CustomerInfoDto,
    shippingAddress: AddressDto,
    billingAddress: AddressDto,
    amountToBill: BigDecimal,
    lines: List[PricedOrderLineDto]
  )

  object OrderPlacedDto {

    /// Convert a OrderPlaced object into the corresponding DTO.
    /// Used when exporting from the domain to the outside world.
    val fromDomain: OrderPlaced => OrderPlacedDto =
      domainObj => {
        val order = domainObj.pricedOrder

        OrderPlacedDto(
          orderId = order.orderId.value,
          customerInfo = CustomerInfoDto.fromCustomerInfo(order.customerInfo),
          shippingAddress = AddressDto.fromAddress(order.shippingAddress),
          billingAddress = AddressDto.fromAddress(order.billingAddress),
          amountToBill = order.amountToBill.value,
          lines = order.lines.map(PricedOrderLineDto.fromDomain)
        )
      }
  }

//===============================================
// DTO for BillableOrderPlaced event
//===============================================

/// Event to send to billing context
  case class BillableOrderPlacedDto(
    orderId: String,
    billingAddress: AddressDto,
    amountToBill: BigDecimal
  )

  object BillableOrderPlacedDto {

    /// Convert a BillableOrderPlaced object into the corresponding DTO.
    /// Used when exporting from the domain to the outside world.
    def fromDomain(domainObj: BillableOrderPlaced): BillableOrderPlacedDto =
      BillableOrderPlacedDto(
        orderId = domainObj.orderId.value,
        billingAddress = AddressDto.fromAddress(domainObj.billingAddress),
        amountToBill = domainObj.amountToBill.value
      )
  }

//===============================================
// DTO for OrderAcknowledgmentSent event
//===============================================

/// Event to send to other bounded contexts
  case class OrderAcknowledgmentSentDto(
    orderId: String,
    emailAddress: String
  )

  object OrderAcknowledgmentSentDto {

    /// Convert a OrderAcknowledgmentSent object into the corresponding DTO.
    /// Used when exporting from the domain to the outside world.
    val fromDomain: OrderAcknowledgmentSent => OrderAcknowledgmentSentDto =
      domainObj =>
        OrderAcknowledgmentSentDto(
          orderId = domainObj.orderId.value,
          emailAddress = domainObj.emailAddress.value
        )
  }
//===============================================
// DTO for PlaceOrderEvent
//===============================================

/// Use a dictionary representation of a PlaceOrderEvent, suitable for JSON
/// See "Serializing Records and Choice Types Using Maps" in chapter 11
  type PlaceOrderEventDto = (String, AnyRef)

  object PlaceOrderEventDto {

    /// Convert a PlaceOrderEvent into the corresponding DTO.
    /// Used when exporting from the domain to the outside world.
    val fromDomain: PlaceOrderEvent => PlaceOrderEventDto = {
      case op: OrderPlaced =>
        "OrderPlaced" -> OrderPlacedDto.fromDomain(op)
      case bop: BillableOrderPlaced =>
        "BillableOrderPlaced" -> BillableOrderPlacedDto.fromDomain(bop)
      case as: OrderAcknowledgmentSent =>
        "OrderAcknowledgmentSent" -> OrderAcknowledgmentSentDto.fromDomain(as)
    }
  }
//===============================================
// DTO for PlaceOrderError
//===============================================

  case class PlaceOrderErrorDto(
    code: String,
    message: String
  )

  object PlaceOrderErrorDto {

    val fromDomain: PlaceOrderError => PlaceOrderErrorDto = {
      case ValidationError(msg)         => PlaceOrderErrorDto("ValidationError", msg)
      case PricingError(msg)            => PlaceOrderErrorDto("ValidationError", msg)
      case RemoteServiceError(info, ex) => PlaceOrderErrorDto("RemoteServiceError", s"${info.name}: ${ex.getMessage}")
    }
  }

}
