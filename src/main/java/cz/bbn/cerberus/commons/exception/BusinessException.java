package cz.bbn.cerberus.commons.exception;

/**
 * Exception means that user tries to do something forbidden. Usual cause is not that code cant continue,
 * but is its forbidden. This problem should be able fix user by changing input for program.
 * eg.: validations, updating entry that does not exist, too low permissions
 */
public class BusinessException extends AppException {

    public BusinessException(ErrorInterface errorInterface, Object... params) {
        super(errorInterface, params);
    }

    public BusinessException(ErrorInterface errorInterface, Throwable e, Object... params) {
        super(errorInterface, e, params);
    }

}
