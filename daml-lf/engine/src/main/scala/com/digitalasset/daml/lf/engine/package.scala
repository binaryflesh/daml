package com.digitalasset.daml.lf

import java.util

package object engine {

  def ArrayList[X](as: X*): util.ArrayList[X] = {
    val a = new util.ArrayList[X](as.length)
    as.foreach(a.add)
    a
  }

}
