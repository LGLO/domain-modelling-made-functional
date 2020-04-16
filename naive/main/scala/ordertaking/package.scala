package ordertaking

import scala.util.matching.Regex
import ordertaking._

object ordertaking {
// ===============================
// Simple types and constrained types related to the OrderTaking domain.
//
// E.g. Single case discriminated unions (aka wrappers), enums, etc
// ===============================

  // Constrained to be 50 chars or less, not null
  case class String50 private (value: String) extends AnyVal

  object String50 {

    // Create an String50 from a string
    // Return Error if input is null, empty, or length > 50
    def create(fieldName: String, str: String) =
      ConstrainedType.createString(fieldName, String50.apply, 50, str)

    // Create an String50 from a string
    // Return None if input is null, empty.
    // Return error if length > maxLen
    // Return Some if the input is valid
    def createOption(fieldName: String, str: String) =
      ConstrainedType.createStringOption(fieldName, String50.apply, 50, str)
  }

  // An email address
  //type EmailAddress = private EmailAddress of string
  case class EmailAddress private (value: String) extends AnyVal

  object EmailAddress {

    // Create an EmailAddress from a string
    // Return Error if input is null, empty, or doesn't have an "@" in it
    def create(fieldName: String, str: String) = {
      val pattern = ".+@.+".r // anything separated by an "@"
      ConstrainedType.createLike(fieldName, EmailAddress.apply, pattern, str)
    }
  }

  // A zip code
  //type ZipCode = private ZipCode of string
  case class ZipCode private (value: String) extends AnyVal

  object ZipCode {

    // Create a ZipCode from a string
    // Return Error if input is null, empty, or doesn't have 5 digits
    def create(fieldName: String, str: String) = {
      val pattern = "\\d{5}".r
      ConstrainedType.createLike(fieldName: String, ZipCode.apply, pattern, str)
    }
  }

  // An Id for Orders. Constrained to be a non-empty string < 10 chars
  //type OrderId = private OrderId of string
  case class OrderId private (value: String) extends AnyVal

  object OrderId {

    // Create an OrderId from a string
    // Return Error if input is null, empty, or length > 50
    def create(fieldName: String, str: String) =
      ConstrainedType.createString(fieldName, OrderId.apply, 50, str)
  }

  // An Id for OrderLines. Constrained to be a non-empty string < 10 chars
  //type OrderLineId = private OrderLineId of string
  case class OrderLineId private (value: String) extends AnyVal

  object OrderLineId {

    // Create an OrderLineId from a string
    // Return Error if input is null, empty, or length > 50
    def create(fieldName: String, str: String) =
      ConstrainedType.createString(fieldName: String, OrderLineId.apply, 50, str)
  }

  // A ProductCode is either a Widget or a Gizmo
  sealed trait ProductCode {
    def value: String
  }
  // The codes for Widgets start with a "W" and then four digits
  //type WidgetCode = private WidgetCode of string
  case class WidgetCode private (value: String) extends ProductCode
  // The codes for Gizmos start with a "G" and then three digits.
  //type GizmoCode = private GizmoCode of string
  case class GizmoCode private (value: String) extends ProductCode

  object ProductCode {

    // Create an ProductCode from a string
    // Return Error if input is null, empty, or not matching pattern
    def create(fieldName: String, code: String): Result[String, ProductCode] =
      if (code == null || code.isEmpty())
        Error(s"$fieldName: Must not be null or empty")
      else if (code.startsWith("W"))
        WidgetCode.create(fieldName, code)
      else if (code.startsWith("G"))
        GizmoCode.create(fieldName, code)
      else
        Error(s"$fieldName: Format not recognized '$code'")
  }

  object WidgetCode {

    // Create an WidgetCode from a string
    // Return Error if input is null. empty, or not matching pattern
    def create(fieldName: String, code: String) = {
      // The codes for Widgets start with a "W" and then four digits
      val pattern = "W\\d{4}".r
      ConstrainedType.createLike(fieldName, WidgetCode.apply, pattern, code)
    }
  }

  object GizmoCode {

    // Create an GizmoCode from a string
    // Return Error if input is null, empty, or not matching pattern
    def create(fieldName: String, code: String) = {
      // The codes for Gizmos start with a "G" and then three digits.
      def pattern = "G\\d{3}".r
      ConstrainedType.createLike(fieldName, GizmoCode.apply, pattern, code)
    }
  }

// A Quantity is either a Unit or a Kilogram
  sealed trait OrderQuantity {
    def value: BigDecimal
  }

  // Constrained to be a integer between 1 and 1000
  case class UnitQuantity private (v: Int) extends OrderQuantity {
    def value: BigDecimal = BigDecimal(v)
  }

  // Constrained to be a decimal between 0.05 and 100.00
  case class KilogramQuantity private (v: BigDecimal) extends OrderQuantity {
    def value: BigDecimal = v
  }

  object UnitQuantity {

    // Create a UnitQuantity from a int
    // Return Error if input is not an integer between 1 and 1000
    def create(fieldName: String, v: Int): Result[String, UnitQuantity] =
      ConstrainedType.createInt(fieldName, UnitQuantity.apply, 1, 1000, v)
  }

  object KilogramQuantity {

    // Create a KilogramQuantity from a decimal.
    // Return Error if input is not a decimal between 0.05 and 100.00
    def create(fieldName: String, v: BigDecimal): Result[String, KilogramQuantity] =
      ConstrainedType.createDecimal(fieldName, KilogramQuantity.apply, BigDecimal(0.5), BigDecimal(100), v)
  }

  object OrderQuantity {

    // Create a OrderQuantity from a productCode and quantity
    def create(fieldName: String, productCode: ProductCode, quantity: BigDecimal) = productCode match {
      case WidgetCode(_) => UnitQuantity.create(fieldName, quantity.toInt)
      case GizmoCode(_)  => KilogramQuantity.create(fieldName, quantity)
    }
  }

  // Constrained to be a decimal between 0.0 and 1000.00
  case class Price private (value: BigDecimal) extends AnyVal

  object Price {

    // Create a Price from a decimal.
    // Return Error if input is not a decimal between 0.0 and 1000.00
    def create(v: BigDecimal): Result[String, Price] =
      ConstrainedType.createDecimal("Price", Price.apply, BigDecimal(0.0), BigDecimal(1000), v)

    // Create a Price from a decimal.
    // Throw an exception if out of bounds. This should only be used if you know the value is valid.
    def unsafeCreate(v: BigDecimal): Price =
      create(v) match {
        case Ok(price)  => price
        case Error(msg) => throw new Exception(s"Not expecting Price to be out of bounds: $msg")
      }

    // Multiply a Price by a decimal qty.
    // Return Error if new price is out of bounds.
    def multiply(qty: BigDecimal, p: Price) =
      create(p.value * qty)
  }

  // Constrained to be a decimal between 0.0 and 10000.00
  case class BillingAmount private (value: BigDecimal) extends AnyVal

  object BillingAmount {

    // Create a BillingAmount from a decimal.
    // Return Error if input is not a decimal between 0.0 and 10000.00
    def create(v: BigDecimal): Result[String, BillingAmount] =
      ConstrainedType.createDecimal("BillingAmount", BillingAmount.apply, BigDecimal(0.0), BigDecimal(10000.0), v)

    // Sum a list of prices to make a billing amount
    // Return Error if total is out of bounds
    def sumPrices(prices: List[Price]) =
      create(prices.map(_.value).sum)
  }

  // Represents a PDF attachment
  case class PdfAttachment(name: String, bytes: Array[Byte])

  // ===============================
  // Reusable constructors and getters for constrained types
  // ===============================

  // Useful functions for constrained types
  object ConstrainedType {

    // Create a constrained string using the constructor provided
    // Return Error if input is null, empty, or length > maxLen
    def createString[T](fieldName: String, ctor: String => T, maxLen: Int, str: String): Result[String, T] =
      if (str == null || str.isEmpty)
        Error(s"$fieldName must not be null or empty")
      else if (str.length > maxLen)
        Error(s"$fieldName must not be more than $maxLen chars")
      else
        Ok(ctor(str))

    // Create a optional constrained string using the constructor provided
    // Return None if input is null, empty.
    // Return error if length > maxLen
    // Return Some if the input is valid
    def createStringOption[T](
      fieldName: String,
      ctor: String => T,
      maxLen: Int,
      str: String
    ): Result[String, Option[T]] =
      if (str == null || str.isEmpty)
        Ok(None)
      else if (str.length > maxLen)
        Error(s"$fieldName must not be more than $maxLen chars")
      else
        Ok(Some(ctor(str)))

    // Create a constrained integer using the constructor provided
    // Return Error if input is less than minVal or more than maxVal
    def createInt[T](fieldName: String, ctor: Int => T, minVal: Int, maxVal: Int, i: Int): Result[String, T] =
      if (i < minVal)
        Error(s"$fieldName: Must not be less than $minVal")
      else if (i > maxVal)
        Error(s"$fieldName: Must not be greater than $maxVal")
      else
        Ok(ctor(i))

    // Create a constrained decimal using the constructor provided
    // Return Error if input is less than minVal or more than maxVal
    def createDecimal[T](
      fieldName: String,
      ctor: BigDecimal => T,
      minVal: BigDecimal,
      maxVal: BigDecimal,
      i: BigDecimal
    ): Result[String, T] =
      if (i < minVal)
        Error(s"$fieldName: Must not be less than $minVal")
      else if (i > maxVal)
        Error(s"$fieldName: Must not be greater than $maxVal")
      else
        Ok(ctor(i))

    // Create a constrained string using the constructor provided
    // Return Error if input is null. empty, or does not match the regex pattern
    def createLike[T](fieldName: String, ctor: String => T, pattern: Regex, str: String): Result[String, T] =
      if (str == null || str.isEmpty)
        Error(s"$fieldName: Must not be null or empty")
      else if (pattern.matches(str))
        Ok(ctor(str))
      else
        Error(s"$fieldName: '$str' must match the pattern '$pattern'")

  }

// ==================================
// Common compound types used throughout the OrderTaking domain
//
// Includes: customers, addresses, etc.
// Plus common errors.
//
// ==================================

// ==================================
// Customer-related types
// ==================================

  case class PersonalName(
    firstName: String50,
    lastName: String50
  )

  case class CustomerInfo(
    name: PersonalName,
    emailAddress: EmailAddress
  )

// ==================================
// Address-related
// ==================================

  case class Address(
    addressLine1: String50,
    addressLine2: Option[String50],
    addressLine3: Option[String50],
    addressLine4: Option[String50],
    city: String50,
    zipCode: ZipCode
  )

// ==================================
// Product-related types
// ==================================

// Note that the definition of a Product is in a different bounded
// context, and in this context, products are only represented by a ProductCode
// (see the SimpleTypes module).
}
