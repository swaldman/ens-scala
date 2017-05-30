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

final object BidStatus {
  final case object Open            extends BidStatus( 0 )
  final case object Auction         extends BidStatus( 1 )
  final case object Owned           extends BidStatus( 2 )
  final case object Forbidden       extends BidStatus( 3 )
  final case object Reveal          extends BidStatus( 4 )
  final case object NotYetAvailable extends BidStatus( 5 )

  val byCode = {
    val statuses = Open :: Auction :: Owned :: Forbidden :: Reveal :: NotYetAvailable :: Nil
      statuses.map( status => ( status.code, status ) ).toMap
  }
}
sealed abstract class BidStatus( val code : Int );
