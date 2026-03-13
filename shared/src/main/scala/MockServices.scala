package mock

import domain.*
import scala.concurrent.{ExecutionContext, Future}

final class CartService:

  def getCartItems(userId: String): CartItems =
    println(s"[CartService] getCartItems(userId=$userId) を呼び出しました")
    CartItems(List(CartItem("product-A", 2), CartItem("product-B", 1)))

final class InventoryService(using ExecutionContext):
  private var reserved: Set[String] = Set.empty

  def getProducts(cartItems: CartItems): Products =
    println(s"[InventoryService] getProducts(${cartItems.items.size}件) を呼び出しました")
    Products(cartItems.items.map(item => Product(item.productId, 500.0)))

  def reserve(productIds: List[String]): Future[Either[String, Unit]] = Future {
    callWithLog("InventoryService", "reserve", s"productIds=${productIds.mkString(",")}") match
      case Left(err) => Left(err)
      case Right(_) =>
        reserved = reserved ++ productIds
        Right(())
  }

  def release(productIds: List[String]): Future[Either[String, Unit]] = Future {
    callWithLog(
      "InventoryService",
      "release",
      s"productIds=${productIds.mkString(",")}",
      canFail = false
    ) match
      case Left(err) => Left(err)
      case Right(_) =>
        reserved = reserved -- productIds
        Right(())
  }

final class PaymentService(using ExecutionContext):
  private var charged = false

  def charge(userId: String, amount: Double): Future[Either[String, Unit]] = Future {
    callWithLog("PaymentService", "charge", s"userId=$userId, amount=$amount") match
      case Left(err) => Left(err)
      case Right(_) =>
        if charged then Left("already charged")
        else
          charged = true
          Right(())
  }

  def refund(userId: String, amount: Double): Future[Either[String, Unit]] = Future {
    callWithLog(
      "PaymentService",
      "refund",
      s"userId=$userId, amount=$amount",
      canFail = false
    ) match
      case Left(err) => Left(err)
      case Right(_) =>
        if charged then charged = false
        Right(())
  }

final class OrderService(using ExecutionContext):
  private var lastOrder: Option[OrderEntity] = None

  def createOrder(userId: String, cartItems: CartItems): Future[Either[String, OrderEntity]] =
    Future {
      callWithLog(
        "OrderService",
        "createOrder",
        s"userId=$userId, items=${cartItems.items.size}件"
      ) match
        case Left(err) => Left(err)
        case Right(_) =>
          val entity = OrderEntity(java.util.UUID.randomUUID().toString, userId, cartItems)
          lastOrder = Some(entity)
          Right(entity)
    }

  def cancelOrder(): Future[Either[String, Unit]] = Future {
    callWithLog("OrderService", "cancelOrder", canFail = false) match
      case Left(err) => Left(err)
      case Right(_) =>
        lastOrder = None
        Right(())
  }

final class ShippingService(using ExecutionContext):
  private var shipped = false

  def ship(orderId: String): Future[Either[String, Unit]] = Future {
    callWithLog("ShippingService", "ship", s"orderId=$orderId") match
      case Left(err) => Left(err)
      case Right(_) =>
        if shipped then Left("already shipped")
        else
          shipped = true
          Right(())
  }

  def cancel(orderId: String): Future[Either[String, Unit]] = Future {
    callWithLog("ShippingService", "cancel", s"orderId=$orderId", canFail = false) match
      case Left(err) => Left(err)
      case Right(_) =>
        if shipped then shipped = false
        Right(())
  }

final class NotificationService(using ExecutionContext):

  def sendOrderConfirmation(userId: String, orderId: String): Future[Either[String, Unit]] =
    Future {
      callWithLog(
        "NotificationService",
        "sendOrderConfirmation",
        s"userId=$userId, orderId=$orderId"
      ) match
        case Left(err) => Left(err)
        case Right(_)  => Right(())
    }
