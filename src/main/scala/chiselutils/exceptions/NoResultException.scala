package chiselutils.exceptions

import Chisel._

class NoResultException( message : String = null, cause : Throwable = null ) extends
    RuntimeException( NoResultException.defaultMessage( message, cause ), cause)

object NoResultException {
  def defaultMessage(message: String, cause: Throwable) =
    if (message != null) message
    else if (cause != null) cause.toString()
    else null

  def apply( message : String ) =
    throw new NoResultException( message )
}

