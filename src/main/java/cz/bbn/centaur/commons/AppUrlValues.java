package cz.bbn.cerberus.commons;

public class AppUrlValues {

    public static final String HEARTBEAT = "/login/HEARTBEAT/**";
    public static final String ACTUATOR = "/actuator/**";

    public static final String LOGIN_PROCESSING_URL = "/login";
    public static final String LOGIN_FAILURE_URL = "/login?error";
    public static final String LOGOUT_SUCCESS_URL = "/dashboard";

    private AppUrlValues() {
    }
}
