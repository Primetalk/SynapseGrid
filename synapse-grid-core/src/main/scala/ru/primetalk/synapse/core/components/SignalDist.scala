package ru.primetalk.synapse.core.components

/** Signal for remote transfer. The real contacts are not quite well serializable (see Contact for details).
  * Thus we use the number of the contact in system's index.
  */
case class SignalDist(contactId: Int, data: AnyRef)
