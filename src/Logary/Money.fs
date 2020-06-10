namespace Logary

type Money = Gauge

[<AutoOpen>]
module Money =
  let money (currency: Currency) (amount: float): Money =
    Gauge (Value.Float amount, U.Currency currency)

  let USD amount: Money = money Currency.USD amount
  let EUR amount: Money = money Currency.EUR amount
