package cz.bbn.cerberus.commons.exception;

/**
 * Is thrown when cause of error is unknown or system cant recover from error (changing any parameter wont help).
 * Does not matter how many times or with what params is code started it always fails.
 * Only changes in code or configuration can help.
 * eg.: remote system did not respond, missing configuration parameter
 */
public class SystemException extends AppException {

    public SystemException(ErrorInterface errorInterface, Object... params) {
        super(errorInterface, params);
    }

    public SystemException(ErrorInterface errorInterface, Throwable e, Object... params) {
        super(errorInterface, e, params);
    }
}
