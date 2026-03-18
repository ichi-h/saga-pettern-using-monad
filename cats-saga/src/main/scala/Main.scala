import cats.effect.unsafe.implicits.global
import domain.*
import mock.*

@main def runCheckoutDemo(): Unit =
  val userId = "user-123" // Mock user ID

  val cart         = CartServiceIO()
  val inventory    = InventoryServiceIO()
  val payment      = PaymentServiceIO()
  val order        = OrderServiceIO()
  val shipping     = ShippingServiceIO()
  val notification = NotificationServiceIO()
  val _services    = (payment, order, shipping, notification)

  val flow: Saga[OrderEntity] = for {
    cartItems <- Saga.step(
      "get_cart_items",
      () => cart.getCartItems(userId),
      Saga.noCompensation
    )
    products <- Saga.step(
      "get_products",
      () => inventory.getProducts(cartItems),
      Saga.noCompensation
    )
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

  val (state, result) = flow.value.run(SagaState.empty).unsafeRunSync()
  result match
    case Right(order) =>
      println(s"Checkout succeeded: Order ID ${order.id}\nSteps: \n${state.stepsSummary}")
    case Left(error) =>
      state.compensate().value.unsafeRunSync() match
        case Right(finalState) =>
          println(s"Checkout failed: $error\nCompensation succeeded.\nSteps: \n${finalState.stepsSummary}")
        case Left(compError) =>
          println(s"Checkout failed: $error\nCompensation failed: $compError\nSteps: \n${state.stepsSummary}")
