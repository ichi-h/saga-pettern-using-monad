package mock

import cats.data.EitherT
import cats.effect.IO
import domain.*

private def liftIO[A](thunk: => A): EitherT[IO, String, A] =
  EitherT.liftF(IO.delay(thunk))

private def delayEither[A](thunk: => Either[String, A]): EitherT[IO, String, A] =
  EitherT(IO.delay(thunk))

final class CartServiceIO:

  def getCartItems(userId: String): EitherT[IO, String, CartItems] =
    liftIO {
      println(s"[CartService] getCartItems(userId=$userId) を呼び出しました")
      CartItems(List(CartItem("product-A", 2), CartItem("product-B", 1)))
    }

final class InventoryServiceIO:
  private var reserved: Set[String] = Set.empty

  def getProducts(cartItems: CartItems): EitherT[IO, String, Products] =
    liftIO {
      println(s"[InventoryService] getProducts(${cartItems.items.size}件) を呼び出しました")
      Products(cartItems.items.map(item => Product(item.productId, 500.0)))
    }

  def reserve(productIds: List[String]): EitherT[IO, String, Unit] =
    delayEither {
      callWithLog("InventoryService", "reserve", s"productIds=${productIds.mkString(",")}") match
        case Left(err) => Left(err)
        case Right(_) =>
          reserved = reserved ++ productIds
          Right(())
    }

  def release(productIds: List[String]): EitherT[IO, String, Unit] =
    delayEither {
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

final class PaymentServiceIO:
  private var charged = false

  def charge(userId: String, amount: Double): EitherT[IO, String, Unit] =
    delayEither {
      callWithLog("PaymentService", "charge", s"userId=$userId, amount=$amount") match
        case Left(err) => Left(err)
        case Right(_) =>
          if charged then Left("already charged")
          else
            charged = true
            Right(())
    }

  def refund(userId: String, amount: Double): EitherT[IO, String, Unit] =
    delayEither {
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

final class OrderServiceIO:
  private var lastOrder: Option[OrderEntity] = None

  def createOrder(userId: String, cartItems: CartItems): EitherT[IO, String, OrderEntity] =
    delayEither {
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

  def cancelOrder(): EitherT[IO, String, Unit] =
    delayEither {
      callWithLog("OrderService", "cancelOrder", canFail = false) match
        case Left(err) => Left(err)
        case Right(_) =>
          lastOrder = None
          Right(())
    }

final class ShippingServiceIO:
  private var shipped = false

  def ship(orderId: String): EitherT[IO, String, Unit] =
    delayEither {
      callWithLog("ShippingService", "ship", s"orderId=$orderId") match
        case Left(err) => Left(err)
        case Right(_) =>
          if shipped then Left("already shipped")
          else
            shipped = true
            Right(())
    }

  def cancel(orderId: String): EitherT[IO, String, Unit] =
    delayEither {
      callWithLog("ShippingService", "cancel", s"orderId=$orderId", canFail = false) match
        case Left(err) => Left(err)
        case Right(_) =>
          if shipped then shipped = false
          Right(())
    }

final class NotificationServiceIO:

  def sendOrderConfirmation(userId: String, orderId: String): EitherT[IO, String, Unit] =
    delayEither {
      callWithLog(
        "NotificationService",
        "sendOrderConfirmation",
        s"userId=$userId, orderId=$orderId"
      ) match
        case Left(err) => Left(err)
        case Right(_)  => Right(())
    }
