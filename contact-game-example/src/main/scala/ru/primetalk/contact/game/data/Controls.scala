package ru.primetalk.contact.game.data

sealed trait Control

case object Start extends Control
case object Restart extends Control
