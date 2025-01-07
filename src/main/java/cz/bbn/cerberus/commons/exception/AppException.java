package cz.bbn.cerberus.commons.exception;

import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import java.text.MessageFormat;
import java.util.Arrays;

/**
 * Abstract parent of all exceptions.
 * Requires ErrorCode, that should be unique for each type of situation.
 * On Frontend code is used to determine error message for subject.
 * Message is information for programmer when exception is written to log.
 * List of params is optional and will be used during generation of error message for subject.
 */
@Slf4j
public abstract class AppException extends Exception {

    private final ErrorInterface errorCode;
    private final Object[] params;

    protected AppException(ErrorInterface errorInterface, Object... params) {
        super(getMessage(errorInterface.getError(), params));
        this.errorCode = errorInterface;
        this.params = params;
    }

    protected AppException(ErrorInterface errorInterface, Throwable e, Object... params) {
        super(getMessage(errorInterface.getError(), params), e);
        this.errorCode = errorInterface;
        this.params = params;
    }

    private static String getMessage(String errorCode, Object... params) {
        try {
            return MessageFormat.format(Transl.get(errorCode), params);
        } catch (IllegalArgumentException e) {
            log.error("Wrong formatted error code message: " + errorCode, e);
            return errorCode + ", params: " + Arrays.toString(params);
        }
    }

    public String getParam(int index) {
        return params[index].toString();
    }

    public int getNumberOfParams() {
        return params.length;
    }

    public Object[] getParams() {
        return params;
    }

    public ErrorInterface getErrorCode() {
        return errorCode;
    }
}
