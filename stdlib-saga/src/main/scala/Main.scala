import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import domain.*
import mock.*

given ExecutionContext = ExecutionContext.global

@main def runCheckoutDemo(): Unit =
  val userId = "user-123" // Mock user ID

  val cart         = CartService()
  val inventory    = InventoryService()
  val payment      = PaymentService()
  val order        = OrderService()
  val shipping     = ShippingService()
  val notification = NotificationService()

  val cartItems = cart.getCartItems(userId)
  val products  = inventory.getProducts(cartItems)

  val flow: Saga[OrderEntity] = for {
    _ <- Saga.step(
      "reserve_inventory",
      () => inventory.reserve(cartItems.toProductIds),
      () => inventory.release(cartItems.toProductIds)
    )
    _ <- Saga.step(
      "charge_payment",
      () => payment.charge(userId, products.totalPrice),
      () => payment.refund(userId, products.totalPrice)
    )
    orderEntity <- Saga.step(
      "create_order",
      () => order.createOrder(userId, cartItems),
      () => order.cancelOrder()
    )
    _ <- Saga.step(
      "create_shipment",
      () => shipping.ship(orderEntity.id),
      () => shipping.cancel(orderEntity.id)
    )
    _ <- Saga.step(
      "send_notification",
      () => notification.sendOrderConfirmation(userId, orderEntity.id),
      Saga.noCompensation
    )
  } yield orderEntity

  Await.result(
    flow.exec(SagaState.empty),
    scala.concurrent.duration.Duration.Inf
  ) match
    case SagaOutcome.Succeeded(state, orderEntity) =>
      println(s"Checkout succeeded: Order ID ${orderEntity.id}\nSteps: \n${state.stepsSummary}")
    case SagaOutcome.Compensated(state, errorMessage) =>
      println(
        s"Checkout failed: $errorMessage\nCompensation succeeded.\nSteps: \n${state.stepsSummary}"
      )
    case SagaOutcome.CompensationFailed(state, errorMessage, compError) =>
      println(
        s"Checkout failed: $errorMessage\nCompensation failed: $compError\nSteps: \n${state.stepsSummary}"
      )
