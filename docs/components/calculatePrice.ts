export const epsilon = 0.0000001;

type Money = Readonly<{ amount: number; currency: 'EUR'}>

export function money(amount: number, currency: 'EUR' = "EUR"): Money {
  if (amount == null) throw new Error("Missing amount, received=" + amount)
  if (typeof amount !== 'number') throw new Error("Missing amount, received=" + typeof amount)
  if (currency == null) throw new Error("Missing currency, received=" + currency)
  if (Number.isNaN(amount) || !Number.isFinite(amount)) throw new Error("Amount is NaN or infinity. received=" + amount)
  if (amount < 100 && Math.abs(amount) >= epsilon) throw new Error("You should pass the EUR amount * 100 (or zero) into this function, but got="+String(amount))
  return { amount, currency }
}

export function formatMoney({ amount, currency }: Money) {
  return `${amount / 100.0} ${currency}`
}

export function equal(m1: Money, m2: Money) {
  return Math.abs(m1.amount - m2.amount) < epsilon
    && m1.currency === m2.currency;
}

export const ContinuousRebate = 0.6

/**
 * Calculates the purchase price.
 * @param {Number} cores Integer
 * @param {Number} devs Integer
 * @param {Number} years Integer
 * @param {Number} continuousRebate Float
 * @returns {totalExclVAT, total: { amount: 100xEUR, currency: "EUR"}, nextYear: Money}
 */
export function calculatePrice(cores: number, devs: number, years: number, continuousRebate: number, chargeVAT: boolean = false) {
  if (!cores || !devs || !years || !continuousRebate) throw new Error("Missing parameters to 'calculatePrice'");
  if (cores < 1 || devs < 1 || years < 1 || continuousRebate >= 1.0 || continuousRebate < 0) throw new Error(`Bad parameters cores=${cores}, devs=${devs}, years=${years}, cr=${continuousRebate}.`);
  const oneYearBase = (cores * 100 + devs * 20 + 250) * 100,
        vatRate = chargeVAT ? 0.25 : 0.0;

  const oneYearExclVAT = oneYearBase,
        oneYearVAT = vatRate * oneYearExclVAT,
        oneYear = money(oneYearExclVAT + oneYearVAT),
        nextYearExclVAT = oneYearBase * (1 - continuousRebate),
        nextYearVAT = vatRate * nextYearExclVAT,
        nextYear = money(nextYearExclVAT + nextYearVAT),
        totalExclVAT = oneYearExclVAT + (years - 1) * nextYearExclVAT,
        totalVAT = vatRate * totalExclVAT,
        total = money(totalExclVAT + totalVAT);

  return {
    oneYearExclVAT: money(oneYearExclVAT),
    oneYearVAT: money(oneYearVAT),
    oneYear,
    totalExclVAT: money(totalExclVAT),
    totalVAT: money(totalVAT),
    total,
    nextYearExclVAT: money(nextYearExclVAT),
    nextYearVAT: money(nextYearVAT),
    nextYear,
    continuousRebate,
    cores,
    devs,
    years,
    chargeVAT,
    vatRate
  }
}