package domain

case class CartItem(productId: String, quantity: Int)

case class CartItems(items: List[CartItem]):
  def toProductIds: List[String] = items.map(_.productId)

case class Product(id: String, price: Double)

case class Products(products: List[Product]):
  def totalPrice: Double = products.map(_.price).sum

case class OrderEntity(id: String, userId: String, items: CartItems)
