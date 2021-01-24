package ru.primetalk.synapse.core.dsl

def constF[A, B](b: B): A => B = _ => b
