/*
 * Distributed as part of ens-scala v.0.0.9
 *
 * Copyright (C) 2019 Machinery For Change, LLC
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php
 *
 */

package com.mchange.sc.v2.ens



// Taken directly from
//     https://github.com/ethereum/ens/blob/7e377df83f733a4213f123602239fad073bdbaa7/contracts/HashRegistrarSimplified.sol
//
// State transitions for names:
//   Open -> Auction (startAuction)
//   Auction -> Reveal
//   Reveal -> Owned
//   Reveal -> Open (if nobody bid)
//   Owned -> Open (releaseDeed or invalidateName)

final object NameStatus {
  final case object Open            extends NameStatus( 0 )
  final case object Auction         extends NameStatus( 1 )
  final case object Owned           extends NameStatus( 2 )
  final case object Forbidden       extends NameStatus( 3 )
  final case object Reveal          extends NameStatus( 4 )
  final case object NotYetAvailable extends NameStatus( 5 )

  val byCode = {
    val statuses = Open :: Auction :: Owned :: Forbidden :: Reveal :: NotYetAvailable :: Nil
      statuses.map( status => ( status.code, status ) ).toMap
  }
}
sealed abstract class NameStatus( val code : Int );
