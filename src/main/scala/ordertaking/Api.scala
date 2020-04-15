package ordertaking

import ordertaking._
import Dto._
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

  type PlaceOrderApi = HttpRequest => IO[Throwable, HttpResponse]

  // =============================
  // Implementation
  // =============================

  // setup dummy dependencies

  val checkProductCodeExists: Implementation.CheckProductCodeExists = _ => true

  val checkAddressExists: Implementation.CheckAddressExists =
    unvalidatedAddress => IO.succeed(Implementation.CheckedAddress(unvalidatedAddress))

  val getProductPrice: Implementation.GetProductPrice =
    productCode => Price.unsafeCreate(BigDecimal(1))

  val createOrderAcknowledgmentLetter: Implementation.CreateOrderAcknowledgmentLetter =
    pricedOrder => Implementation.HtmlString("some text")

  val sendOrderAcknowledgment: Implementation.SendOrderAcknowledgment =
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

  val placeOrderApi: PlaceOrderApi =
    request => {
      // following the approach in "A Complete Serialization Pipeline" in chapter 11
      // start with a string
      val orderFormJson = request.body
      val orderForm: OrderFormDto = ??? //JsonConvert.DeserializeObject<OrderFormDto>(orderFormJson)
      // convert to domain object
      val unvalidatedOrder = orderForm.toUnvalidatedOrder

      // setup the dependencies. See "Injecting Dependencies" in chapter 9
      val workflow = Implementation.placeOrder(checkProductCodeExists)(checkAddressExists)(getProductPrice)(
        createOrderAcknowledgmentLetter
      )(sendOrderAcknowledgment)

      // now we are in the pure domain
      val asyncResult = workflow(unvalidatedOrder)

      // now convert from the pure domain back to a HttpResponse
      workflowResultToHttpReponse(asyncResult)
    }
}
