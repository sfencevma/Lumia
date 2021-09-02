package Lumia

import org.scalatest._
import chisel3._
import chisel3.util._
import Lumia.common._
import freechips.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.tile._

object LumiaTestUnit {
    private def augment(tp: LumiaTileParams)(implicit p: Parameters): Parameters = p.alterPartial {
        case LumiaTileKey => tp
    }
    def getLumiaParameters(configName: String, configPackage: String = "Lumia.system"): Parameters = {
        val fullConfigName = configPackage + "." + configName
        val origParams: Parameters = try {
            Class.forName(fullConfigName).newInstance().asInstanceOf[Config] ++ Parameters.empty
        } catch {
            case e: java.lang.ClassNotFoundException =>
                throw new Exception(s"""Unable to find config $fullConfigName""", e)
        }
        val cloverTileParams = origParams(LumiaTileKey)
        val outParams = augment(cloverTileParams)(origParams)
        outParams
    }

}