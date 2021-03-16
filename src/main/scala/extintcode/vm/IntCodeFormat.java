package extintcode.vm;

import scala.Function3;

import java.io.IOException;
import java.nio.file.Path;

public enum IntCodeFormat {

    BINARY("ic", IntCodeInterpreter::createBinary),
    PLAIN("ints", IntCodeInterpreter::createPlain);
    
    public final String extension;
    private final Function3<Path, Long, Long, IntCodeInterpreter> factory;

    IntCodeFormat(String extension, Function3<Path, Long, Long, IntCodeInterpreter> factory) {
        this.extension = extension;
        this.factory = factory;
    }

    public IntCodeInterpreter createInterpreter(Path path, long flagSet, long flagUnset) throws IOException {
        return factory.apply(path, flagSet, flagUnset);
    }

    @Override
    public String toString() {
        return name().toLowerCase();
    }
}
