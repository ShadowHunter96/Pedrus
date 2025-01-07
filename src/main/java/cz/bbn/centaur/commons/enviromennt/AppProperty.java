package cz.bbn.cerberus.commons.enviromennt;

public enum AppProperty {

    DEBUG("project.debug"),

    VERSION("project.version"),

    NOTIFICATION_DURATION_ERR("project.notification.error.duration"),
    NOTIFICATION_DURATION_WARN("project.notification.warning.duration"),
    NOTIFICATION_DURATION_SUCCESS("project.notification.success.duration"),

    LOCAL_LOGIN_ROLE("project.local.login.role"),
    LOCAL_LOGIN_USER_NAME("project.local.login.user-name"),
    LOCAL_LOGIN_MAIL("project.local.login.mail"),

    PROFILE("spring.profiles.active"),
    SECURITY_PROFILE("project.security.profile"),
    SESSION_TIMEOUT("project.session.timeout.minutes"),
    ENVIRONMENT_NAME("project.env.name"),
    ENVIRONMENT_COLOR("project.env.color"),

    LDAP_ENABLED("project.ldap.login.enabled"),
    LDAP_URL("project.ldap.login.url"),
    LDAP_BASE("project.ldap.login.base"),
    LDAP_USERDN("project.ldap.login.userdn"),
    LDAP_PASSWORD("project.ldap.login.password"),

    HADOOP_PATH("project.hadoop.path"),
    HADOOP_USER("project.hadoop.user"),

    LOCAL_FILE_PATH("project.local.file.path"),

    USER_MESSAGE_COLOR_NORMAL("project.user.message.normal.color"),
    USER_MESSAGE_COLOR_PRIORITY("project.user.message.priority.color"),
    USER_MESSAGE_COLOR_LEVEL1("project.user.message.date.color.level1"),
    USER_MESSAGE_COLOR_LEVEL2("project.user.message.date.color.level2"),
    USER_MESSAGE_COLOR_LEVEL3("project.user.message.date.color.level3"),
    USER_MESSAGE_DAYS_LEVEL1("project.user.message.date.days.level1"),
    USER_MESSAGE_DAYS_LEVEL2("project.user.message.date.days.level2"),
    USER_MESSAGE_DAYS_LEVEL3("project.user.message.date.days.level3"),

    SVN_URL("project.svn.url"),
    SVN_PATH("project.svn.path"),
    SVN_USER("project.svn.user"),
    SVN_PASSWORD("project.svn.password"),

    AZURE_LOGOUT("spring.cloud.azure.active-directory.logout"),

    POHODA_URL("project.pohoda.url"),
    POHODA_USER_PASWORD("project.pohoda.userpassword.base64"),
    POHODA_ICO("project.pohoda.ico"),

    JUSTICE_WSO2_URL("project.justice.wso2.url"),
    JUSTICE_TOKEN("project.justice.token"),
    JUSTICE_URL_BEGINNING("project.justice.link.beginning"),
    JUSTICE_URL_END("project.justice.link.end"),

    DEFAULT_LONGITUDE("project.map.default.longitude"),
    DEFAULT_LATITUDE("project.map.default.latitude"),

    ADIS_URL("project.adis.url"),

    AZURE_LOGIN("spring.cloud.azure.active-directory.enabled"),
    MONITORING_SHOW_STACKTRACE("project.monitoring.show-stacktrace"),

    DEFAULT_COMPANY_SUBJECT("project.default.company.subject"),
    HISTORY_ITEM_COUNT("project.history.item.count"),
    LDAP_PREFIX("project.ldap.role-prefix"),

    PROJECT_URL("project.url"),

    FOOD_VOUCHER_VALUE("project.attendance.food.voucher.value"),

    API_KEY("project.api.key"),

    CONTRACT_SUFFIX("project.contract.suffix");

    private final String key;

    AppProperty(String key) {
        this.key = key;
    }

    public String getKey() {
        return key;
    }
}
