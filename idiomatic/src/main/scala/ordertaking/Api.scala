package ordertaking

import ordertaking._
import Dto._
import Implementation._
import PublicTypes._
import zio.{ IO, UIO }

object Api {

  import java.net.URI

  type JsonString = String

  case class HttpRequest(
    action: String,
    uri: String,
    body: JsonString
  )

/// Very simplified version!
  case class HttpResponse(
    httpStatusCode: Int,
    body: JsonString
  )

  // =============================
  // Implementation
  // =============================

  // setup dummy dependencies

  val checkProductCodeExists: ProductCode => Boolean = _ => true

  val checkAddressExists: UnvalidatedAddress => IO[AddressValidationError, CheckedAddress] =
    unvalidatedAddress => IO.succeed(Implementation.CheckedAddress(unvalidatedAddress))

  val getProductPrice: ProductCode => Price =
    productCode => Price.unsafeCreate(BigDecimal(1))

  val createOrderAcknowledgmentLetter: PricedOrder => HtmlString =
    pricedOrder => Implementation.HtmlString("some text")

  val sendOrderAcknowledgment: OrderAcknowledgment => SendResult =
    orderAcknowledgement => Implementation.Sent

  // -------------------------------
  // workflow
  // -------------------------------

  /// This function converts the workflow output into a HttpResponse
  val workflowResultToHttpReponse: IO[PlaceOrderError, List[PlaceOrderEvent]] => UIO[HttpResponse] =
    result =>
      result.fold(
        err => {
          // turn domain errors into a dto
          val dto = PlaceOrderErrorDto.fromDomain(err)
          // and serialize to JSON
          val json = dto.toString
          HttpResponse(
            httpStatusCode = 401,
            body = json
          )
        },
        events => {
          val dtos = events.map(PlaceOrderEventDto.fromDomain)
          val json = events.mkString

          HttpResponse(
            httpStatusCode = 200,
            body = json
          )
        }
      )

  def placeOrderApi(request: HttpRequest): IO[Throwable, HttpResponse] = {
    // following the approach in "A Complete Serialization Pipeline" in chapter 11
    // start with a string
    val orderFormJson = request.body
    val orderForm: OrderFormDto = ??? //JsonConvert.DeserializeObject<OrderFormDto>(orderFormJson)
    // convert to domain object
    val unvalidatedOrder = orderForm.toUnvalidatedOrder

    // setup the dependencies. See "Injecting Dependencies" in chapter 9
    def workflow(unvalidatedOrder: UnvalidatedOrder) = Implementation.placeOrder(
      checkProductCodeExists,
      checkAddressExists,
      getProductPrice,
      createOrderAcknowledgmentLetter,
      sendOrderAcknowledgment,
      unvalidatedOrder
    )

    // now we are in the pure domain
    val asyncResult = workflow(unvalidatedOrder)

    // now convert from the pure domain back to a HttpResponse
    workflowResultToHttpReponse(asyncResult)
  }
}
