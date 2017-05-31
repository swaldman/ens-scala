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
