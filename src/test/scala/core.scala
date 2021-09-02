package Lumia.system

import Lumia.common._
import freechips.rocketchip.config.{Config}

class LumiaConfig extends Config (
    new WithNMegaLumia(0)
)
