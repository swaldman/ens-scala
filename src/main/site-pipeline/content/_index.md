+++
date = "2017-06-02T00:50:13-07:00"
draft = false
title = "Home"

+++
_ens-scala_ is a simple Scala library for interacting with the [Ethereum Name Service](http://ens.domains/).
It can lookup owners, addresses, and contents in the `.eth` domain, transfer or release names, and manage
participation in auctions to take control of names.

### prerequsites

_ens-scala_ uses [Ethereum's standard jsonrpc api <i class='fa fa-github'></i>](https://github.com/ethereum/wiki/wiki/JSON-RPC).

Your application will require access to a node you trust that offers jsonrpc access.

### getting the library

_ens-scala_ is available on Maven central. Its coordinates (in sbt format) are

    libraryDependencies += "com.mchange" %% "ens-scala" % "@VERSION@"

You can find the source code on github, [swaldman/ens-scala <i class='fa fa-github'></i>](https://github.com/swaldman/ens-scala)

### about

This library is built with (it's really the first test project of) [sbt-ethereum <i class='fa fa-github'></i>](https://github.com/swaldman/sbt-ethereum), 
a Scala-centric development
environment and general Swiss army knife for the Ethereum ecosystem. _sbt-ethereum_ is not quite ready for general use, but it will
be soon. Keep an eye out for it.

_ens-scala's_ author is Steve Waldman &lt;[swaldman@mchange.com](mailto:swaldman@mchange.com)&gt;. He is an asshole,
but you can try to contact him if you want to.

### legal and licence

&copy; 2017 Machinery For Change, Inc.

 ens-scala _is made available for use, modification, and redistribution,
 under the terms of the [Lesser GNU Public License, v.2.1 (LGPL)](http://www.gnu.org/licenses/lgpl-2.1.html) or
 the [Eclipse Public License, v.1.0 (EPL)](http://www.eclipse.org/legal/epl-v10.html),
 at your option._

---

_This documentation site is produced with [hugo](https://gohugo.io), and uses the theme [hugo-theme-docdock <i class='fa fa-github'></i>](https://github.com/vjeantet/hugo-theme-docdock/blob/master/LICENSE.md)
under an [MIT Licence](https://github.com/vjeantet/hugo-theme-docdock/blob/master/LICENSE.md)._
