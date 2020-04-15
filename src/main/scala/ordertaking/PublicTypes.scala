package ordertaking

import ordertaking._
import java.net.URI
import zio.IO

object PublicTypes {

  case class UnvalidatedCustomerInfo(
    firstName: String,
    lastName: String,
    emailAddress: String
  )

  case class UnvalidatedAddress(
    addressLine1: String,
    addressLine2: String,
    addressLine3: String,
    addressLine4: String,
    city: String,
    zipCode: String
  )

  case class UnvalidatedOrderLine(
    orderLineId: String,
    productCode: String,
    quantity: BigDecimal
  )

  case class UnvalidatedOrder(
    orderId: String,
    customerInfo: UnvalidatedCustomerInfo,
    shippingAddress: UnvalidatedAddress,
    billingAddress: UnvalidatedAddress,
    lines: List[UnvalidatedOrderLine]
  )

  // ------------------------------------
  // outputs from the workflow (success case)

  // priced state
  case class PricedOrderLine(
    orderLineId: OrderLineId,
    productCode: ProductCode,
    quantity: OrderQuantity,
    linePrice: Price
  )

  case class PricedOrder(
    orderId: OrderId,
    customerInfo: CustomerInfo,
    shippingAddress: Address,
    billingAddress: Address,
    amountToBill: BillingAmount,
    lines: List[PricedOrderLine]
  )

  /// The possible events resulting from the PlaceOrder workflow
  /// Not all events will occur, depending on the logic of the workflow
  sealed trait PlaceOrderEvent

  /// Event will be created if the Acknowledgment was successfully posted
  case class OrderAcknowledgmentSent(
    orderId: OrderId,
    emailAddress: EmailAddress
  ) extends PlaceOrderEvent

  /// Event to send to shipping context
  case class OrderPlaced(pricedOrder: PricedOrder) extends PlaceOrderEvent

  /// Event to send to billing context
  /// Will only be created if the AmountToBill is not zero
  case class BillableOrderPlaced(
    orderId: OrderId,
    billingAddress: Address,
    amountToBill: BillingAmount
  ) extends PlaceOrderEvent

  // ------------------------------------
  // error outputs

  /// All the things that can go wrong in this workflow
  sealed trait PlaceOrderError
  case class ValidationError(message: String) extends PlaceOrderError
  case class PricingError(message: String) extends PlaceOrderError
  case class ServiceInfo(name: String, endpoint: URI)
  case class RemoteServiceError(service: ServiceInfo, exception: Exception) extends PlaceOrderError

  // ------------------------------------
  // the workflow itself

  type PlaceOrder = UnvalidatedOrder => IO[PlaceOrderError, List[PlaceOrderEvent]]
}
