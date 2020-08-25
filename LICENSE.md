Copyright 2018 Henrik Feldt, Jun and contributors.

Logary is the collective name for:

 - a *logging library* for .Net (Logary-lib) including *adapters*,
 - a *Dash web app* that shows streaming logs in your browser,
 - a number of *target* implementations for the library (things Logary-lib can send logs to),
 - a service called *Rutta* that ships, transforms, enhances or proxies logs,
 - a *Helm chart for Rutta*,
 - a *library façade* for use in your own code, to quickly and easily add structured logging capabilities with no
   dependency on a third party library, and
 - a *JavaScript library* to send logs from web browsers and react-native apps.

If you want to support the eco-system of open source and you're a professional developer using Logary commercially, you
should purchase the commercial license to support Logary's maintenance. For some of Logary, you have purchase
a commercial license, for others, you're don't have to. All of this is detailed below.

All of Logary is under the *Terms of Service* (ToS). These ToS are distinct from the *Commercial License* which is in
turn distinct from any other license applied.

---

# Logary licencing

This section defines the licenses for each subcomponent of Logary.

## Logary Library (Logary-lib)

Present at `src/Logary{,Tests,PerfTests}` and `src/adapters/*`. Each being a .Net library that you link your own
code/software to.

Non-profits and non-profit projects are free to use without payment, subject to the Terms of Service defined below.

For-profit or commercial entities must purchase licenses to use Logary for their production environments, subject to the
Terms of Service defined below.

## Logary Dash

Present at `src/services/Dash` and `src/targets/Logary.Targets.SSE`; a web app and an EventSource/SSE based target.

Non-profits and non-profit projects are free to use without payment, subject to the Terms of Service defined below.

For-profit or commercial entities must purchase licenses to use Dash for their production environments, subject to the
Terms of Service defined below.

## Targets

Present at `src/targets/*`, being .Net libraries that you link your own software to, or add as plugins to Rutta.

Targets incorporated into the Logary code-base cannot be used without using the Logary library, but their code is free
to modify, extend and sell under the *Apache 2.0* license, unless specified otherwise on a per target basis (see below).

The following targets' licenses must always be purchased, independent of the for-profit status of the purchasing entity:

- Mixpanel (under `src/targets/Logary.Targets.Mixpanel`) — commercial user analytics, segmentation
- Opsgenie (under `src/targets/Logary.Targets.Opsgenie`) — commercial alerting and monitoring

All other targets (and their code-bases) are licensed as Apache 2.0 (Appendix A contains links to Apache 2.0).

## Rutta

Present at `src/services/*`, being an executable program and docker container.

Rutta is *dual-licensed* as **GPL v3**, or as **a commercial license**. Purchasing a commercial license for Rutta lets
you use and extend its source code for your own commercial purposes and not open source that change.

Rutta effectively is an explicit exception to the no-commercial-use of the Logary library, since GPL v3 software admits
running it as a server for commercial purposes. No other exception to the Logary-lib license is implied by this fact.

GPL v3 means that when you want to extend Rutta for your own purposes, you **must send a pull request with that change**
in order to make your improvement available for others to use. This is so that free software can continue to be free. If
you don't want to send a pull request with your change, you must purchase the commercial license.

## Library Façade

Present at `src/Logary{,CSharp}.Facade{,.Tests}`. Also called the Façade, in short.

The Façade is **Apache 2.0** licensed.

The *Library Façade* is distinct from the *Façade Adapter*, since the Façade is Apache 2.0-licensed code, whilst the
*Façade Adapter* (in `src/adapters/Logary.Adapters.Facade`) doesn't work without Logary-lib.

## `logary` JavaScript library

See [logary/logary-js/LICENSE](https://github.com/logary/logary-js/blob/master/LICENSE.md).

---

# Terms of Service (ToS)

## 1. Jurisdiction

This software's license shall be interpreted in accordance with the laws of Sweden.

## 2. Submission of Contributions

2.1. When you (Contributor) submit a pull request or otherwise seek to include your code in the Logary project, you
waive all your intellectual property rights, including your copyright and patent claims for the submission. You agree
that the modification that you submit may be relicensed as Logary's authors see fit.

2.2. Unless otherwise stated by Logary's authors, your contribution will be licensed as per the component it is for.

## 3. Limitation of Warranty

3.1. Logary and all of its services, adapters, libraries and related software are provided ‘as is’ and ‘as available’
without warranty of any kind. Your use of the software is solely your responsibility. Logary's authors do not grant you
any warranties, express or implied or otherwise, as to the accessibility, quality, fitness for any particular purpose,
suitability or accuracy of the software, to the extent permitted by applicable law.

3.2. We advice you to ascertain the fitness and functionality of the software yourself by fully testing it for your
purposes and reading its source code.

## 4. Limitation of Liability

4.1. To the extent permitted by applicable law, Logary's authors are not liable for any direct nor indirect loss
suffered by you, unless they has been found guilty of gross negligence. This limitation of liability includes, but is
not limited to, loss of production or sales, loss of profit and cost of capital. Logary's author's liability under this
section shall, in any event, be limited to an amount of half a price base amount according to the 1962:381 law about
common insurance (Sv: Lagen om Allmänna försäkringar).

## 5. Indemnification

You will indemnify and hold Logary's authors harmless from any claim or demand, including reasonable attorneys' fees,
made by any third party due to or arising out of your breach of these Terms, our policies, or your violation of any law
or the rights of a third party, to the extent permitted by applicable law.

## 6. Collection of statistics / environment data

You agree that Logary may collect statistics on its usage and runtime environment (operating system name and version,
IP-address, the hostname that is running Logary components, linked library identifiers, etc), by means of communication
with Logary authors' servers.

## 7. Changes to the terms

Logary's authors may make changes to these Terms from time to time. When we introduce changes we will commit the most
current version to this document, and if a revision of the Terms is material and you have made a purchase of the
commercial license, we will notify you of the new Terms (either by e-mail or a Github notification) as appropriate. If
you do not agree to the modified terms, you should discontinue your use of Logary.

---

# Commercial License

This section defines the terms of the Commercial License.

The Commercial License is subject to the Terms of Service (ToS) above. It applies to a Work or Software, its source code
and the running/deployment of the software.

## 1. Pricing

The commercial license must be purchased in order to apply to the Software.

### 1.1 Initial purchase

The price in EUR (€) is calculated as follows:

    price = C * 100 + D * 20 + 250

where

- `C`: # of cores in total production deployment
- `D`: # of developers owning/working on the software

You can load- and stress-test in a test-/staging-environment, for free, as long as that environment never serves production
traffic.

If you change the number of cores you deploy on, or number of developers, please order a delta to your license.

### 1.2 Subsequent years

You must renew the license(s) yearly, at a 60% discount of the initial price that a new customer would pay at the
instant when the license expires.

So for example, if you have 5 different services, each with access to 10 cores, and your team is 5 backend and 2
frontend developers, you pay 10 \* 100 + 5 \* 20 + 250 = €1,350 euro the first year, and €540 every subsequent year if
the number of cores used in production and the number of developers "owning"/building the software is the same.

### 1.3 Means of Payment

You may choose to pay via invoice (SWIFT/IBAN bank transfer, or via a lightning network invoice), or
through credit/debit card.

### 1.4 Terms of Payment

#### 1.4.1 Invoice

The invoice must be paid within 10 days of its issuance.

#### 1.4.2 Credit/Debit Card

Credit/Debit card payments are immediate.

## 2. Allowances

By purchasing this license you may make changes to the Software without open sourcing or sending a pull request with
those changes. Furthermore, you are free to link the Software to your own software, under the terms of this license.

## 3. Refunds

Unless expressly agreed in writing, before the purchase, there are no refunds, to the extent of the applicable law. If
you are purchasing an individual, you have 14 days right to withdrawal according to [Swedish
law](https://www.konsumenteuropa.se/en/topics/e-commerce/E-commerce-within-the-EU/right-of-withdrawal-within-the-eu/).

## 4. Order a license

Send an e-mail to `henrik@haf.se` with the number of *cores* and *developers* you'd like to purchase a license for.

---

# Appendix A — Apache 2.0 License

Logary Façade

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

---

# Appendix B — GNU Public License v3

From https://www.gnu.org/licenses/gpl-3.0.en.html

Logary Rutta

Copyright (C) 2019 Henrik Feldt

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <https://www.gnu.org/licenses/>.

---

# Appendix C — MIT License

Logary JS

Copyright (C) 2019 Henrik Feldt

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
