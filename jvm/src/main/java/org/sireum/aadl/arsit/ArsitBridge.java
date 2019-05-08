package org.sireum.aadl.arsit;

import org.sireum.None$;
import org.sireum.Some$;

public class ArsitBridge {

    public enum IPCMechanismJava {
        MessageQueue,
        SharedMemory
    }

    public static <T> org.sireum.Option<T> sireumOption(T o) {
        if(o != null) {
            return Some$.MODULE$.apply(o);
        } else {
            return None$.MODULE$.apply();
        }
    }
}