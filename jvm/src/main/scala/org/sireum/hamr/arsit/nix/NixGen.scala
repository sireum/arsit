package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.arsit._
import org.sireum.hamr.ir.Aadl

object NixGen {

  def generate(dirs: ProjectDirectories,
               m: Aadl,
               arsitOptions: Cli.ArsitOption,
               types: AadlTypes,
               previousPhase: Result): ArsitResult = {
    return arsitOptions.platform match {
      case Cli.ArsitPlatform.Linux =>
        ArtNixGen(dirs, m, arsitOptions, types, previousPhase)
      case Cli.ArsitPlatform.Cygwin =>
        ArtNixGen(dirs, m, arsitOptions, types, previousPhase)
      case Cli.ArsitPlatform.MacOS =>
        ArtNixGen(dirs, m, arsitOptions, types, previousPhase)
      case Cli.ArsitPlatform.SeL4 =>
        SeL4NixGen(dirs, m, arsitOptions, types, previousPhase)
      case _ => 
        ArsitResult(
          previousPhase.resources,
          previousPhase.maxPort,
          previousPhase.maxComponent,
          ISZ()
        )
    }
  }
}
