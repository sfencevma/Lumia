package Lumia.common

import Chisel._

import freechips.rocketchip.config.Parameters


/**
 * Lumia module that is used to add parameters to module
 */
abstract class LumiaModule(implicit val p: Parameters) extends Module with HasLumiaCoreParameters

/**
 * Lumia bundle used to add parameters to the object/class/trait
 */
abstract class LumiaBundle(implicit val p: Parameters) extends Bundle with HasLumiaCoreParameters