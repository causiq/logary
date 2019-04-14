const epsilon = 0.0000001;

exports = {}

function money(amount, currency = "EUR") {
  if (amount == null) throw new Error("Missing amount, received=" + amount)
  if (typeof amount !== 'number') throw new Error("Missing amount, received=" + typeof amount)
  if (currency == null) throw new Error("Missing currency, received=" + currency)
  if (Number.isNaN(amount) || !Number.isFinite(amount)) throw new Error("Amount is NaN or infinity. received=" + amount)
  if (amount < 100 && Math.abs(amount) >= epsilon) throw new Error("You should pass the EUR amount * 100 (or zero) into this function, but got="+String(amount))
  return { amount, currency }
}
exports.money = money

function formatMoney({ amount, currency }) {
  return `${amount / 100.0} ${currency}`
}
exports.formatMoney = formatMoney

function equal(m1, m2) {
  return Math.abs(m1.amount - m2.amount) < epsilon
    && m1.currency === m2.currency;
}
exports.equal = equal

const ContinuousRebate = 0.6
exports.ContinuousRebate = ContinuousRebate

/**
 * Calculates the purchase price.
 * @param {Number} cores Integer
 * @param {Number} devs Integer
 * @param {Number} years Integer
 * @param {Number} continuousRebate Float
 * @returns {totalExclVAT, total: { amount: 100xEUR, currency: "EUR"}, nextYear: Money}
 */
function calculatePrice(cores, devs, years, continuousRebate, chargeVAT=false){
  if (!cores || !devs || !years || !continuousRebate) throw new Error("Missing parameters to 'calculatePrice'");
  if (cores < 1 || devs < 1 || years < 1 || continuousRebate >= 1.0 || continuousRebate < 0) throw new Error(`Bad parameters cores=${cores}, devs=${devs}, years=${years}, cr=${continuousRebate}.`);
  const oneYear = (cores * 100 + devs * 20 + 250) * 100,
        vatRate = chargeVAT ? 0.25 : 0.0;

  const totalExclVAT = oneYear * years,
        totalVAT = vatRate * totalExclVAT,
        total = money(totalExclVAT + totalVAT),
        nextYearExclVAT = oneYear * (1 - continuousRebate),
        nextYearVAT = vatRate * nextYearExclVAT,
        nextYear = money(nextYearExclVAT + nextYearVAT);

  return {
    totalExclVAT: money(totalExclVAT),
    totalVAT: money(totalVAT),
    total,
    nextYearExclVAT: money(totalExclVAT),
    nextYearVAT: money(nextYearVAT),
    nextYear,
    chargeVAT
  }
}
exports.calculatePrice = calculatePrice

module.exports = exports