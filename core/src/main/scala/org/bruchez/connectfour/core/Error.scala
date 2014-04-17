package org.bruchez.connectfour.core

sealed trait Error

case class FullColumn(column: Int) extends Error
