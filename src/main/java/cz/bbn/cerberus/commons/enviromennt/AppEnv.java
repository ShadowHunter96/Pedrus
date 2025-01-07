package cz.bbn.cerberus.commons.enviromennt;

import cz.bbn.cerberus.commons.exception.ErrorCode;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;


@Configuration
@Slf4j
@Getter
public class AppEnv {

    private final Environment env;

    private boolean debug;
    private int errorDuration;
    private int warningDuration;
    private int successDuration;
    private int timeout;

    private String userMessageColorLevel1;
    private String userMessageColorLevel2;
    private String userMessageColorLevel3;

    private int userMessageDaysLevel1;
    private int userMessageDaysLevel2;
    private int userMessageDaysLevel3;

    private String userMessageColorPriority;
    private String userMessageColorNormal;

    private String svnUrl;
    private String svnPath;
    private String svnUser;
    private String svnPassword;

    private String pohodaUrl;
    private String pohodaUserPassword;
    private String pohodaIco;

    private String justiceWso2Url;
    private String justiceToken;

    private String justiceUrlBeginning;
    private String justiceUrlEnd;

    private String adisUrl;

    private boolean azureLogin;

    private boolean monitoringShowStackTrace;

    private String defaultCompanySubject;
    private int historyItemCount;

    private String ldapPrefix;

    private String projectUrl;

    private String contractSuffix;

    private String apiKey;

    public AppEnv(Environment env) {
        this.env = env;
        setProperties();
    }

    public String getProperty(AppProperty key) {
        return this.getStringProperty(key);
    }

    public String getStringProperty(AppProperty key) {
        return env.getProperty(key.getKey());
    }

    public String getStringProperty(AppProperty key, String defaultValue) {
        String value = env.getProperty(key.getKey());
        if (StringUtils.isEmpty(value)) {
            return defaultValue;
        }
        return value;
    }

    public boolean getBooleanProperty(AppProperty key) {
        return Boolean.parseBoolean(env.getProperty(key.getKey()));
    }

    public long getLongProperty(AppProperty key, int defValue) {
        return this.getLongProperty(key.getKey(), defValue);
    }

    public int getIntegerProperty(AppProperty key, int defValue) {
        return this.getIntegerProperty(key.getKey(), defValue);
    }

    public double getDoubleProperty(AppProperty key, int defValue) {
        return this.getDoubleProperty(key.getKey(), defValue);
    }

    public boolean getDebug() {
        return this.getBooleanProperty(AppProperty.DEBUG);
    }

    private void setProperties() {
        debug = this.getDebug();
        errorDuration = this.getIntegerProperty(AppProperty.NOTIFICATION_DURATION_ERR, 0);
        warningDuration = this.getIntegerProperty(AppProperty.NOTIFICATION_DURATION_WARN, 5000);
        successDuration = this.getIntegerProperty(AppProperty.NOTIFICATION_DURATION_SUCCESS, 3000);
        timeout = this.getIntegerProperty(AppProperty.SESSION_TIMEOUT, 30);

        userMessageColorPriority = this.getStringProperty(AppProperty.USER_MESSAGE_COLOR_PRIORITY);
        userMessageColorNormal = this.getStringProperty(AppProperty.USER_MESSAGE_COLOR_NORMAL);
        userMessageColorLevel1 = this.getStringProperty(AppProperty.USER_MESSAGE_COLOR_LEVEL1);
        userMessageColorLevel2 = this.getStringProperty(AppProperty.USER_MESSAGE_COLOR_LEVEL2);
        userMessageColorLevel3 = this.getStringProperty(AppProperty.USER_MESSAGE_COLOR_LEVEL3);
        userMessageDaysLevel1 = this.getIntegerProperty(AppProperty.USER_MESSAGE_DAYS_LEVEL1, 1);
        userMessageDaysLevel2 = this.getIntegerProperty(AppProperty.USER_MESSAGE_DAYS_LEVEL2, 3);
        userMessageDaysLevel3 = this.getIntegerProperty(AppProperty.USER_MESSAGE_DAYS_LEVEL3, 7);

        svnUrl = this.getStringProperty(AppProperty.SVN_URL);
        svnPath = this.getStringProperty(AppProperty.SVN_PATH);
        svnUser = this.getStringProperty(AppProperty.SVN_USER);
        svnPassword = this.getStringProperty(AppProperty.SVN_PASSWORD);

        pohodaUrl = this.getStringProperty(AppProperty.POHODA_URL);
        pohodaUserPassword = this.getStringProperty(AppProperty.POHODA_USER_PASWORD);
        pohodaIco = this.getStringProperty(AppProperty.POHODA_ICO);

        justiceWso2Url = this.getStringProperty(AppProperty.JUSTICE_WSO2_URL);
        justiceToken = this.getStringProperty(AppProperty.JUSTICE_TOKEN);

        justiceUrlBeginning = this.getStringProperty(AppProperty.JUSTICE_URL_BEGINNING);
        justiceUrlEnd = this.getStringProperty(AppProperty.JUSTICE_URL_END);

        adisUrl = this.getStringProperty(AppProperty.ADIS_URL);

        azureLogin = this.getBooleanProperty(AppProperty.AZURE_LOGIN);

        monitoringShowStackTrace = this.getBooleanProperty(AppProperty.MONITORING_SHOW_STACKTRACE);

        defaultCompanySubject = this.getStringProperty(AppProperty.DEFAULT_COMPANY_SUBJECT);

        ldapPrefix = this.getStringProperty(AppProperty.LDAP_PREFIX);
        historyItemCount = this.getIntegerProperty(AppProperty.HISTORY_ITEM_COUNT, 10);

        projectUrl = this.getStringProperty(AppProperty.PROJECT_URL);

        contractSuffix = this.getStringProperty(AppProperty.CONTRACT_SUFFIX);

        apiKey = this.getStringProperty(AppProperty.API_KEY);
    }

    private int getIntegerProperty(String key, int defValue) {
        int result;
        String propertyValue = env.getProperty(key);
        try {
            if (propertyValue == null) {
                result = defValue;
            } else {
                result = Integer.parseInt(propertyValue);
            }
        } catch (NumberFormatException e) {
            log.warn(ErrorCode.INVALID_PROPERTY_VALUE.getMessage("integer", propertyValue, key) + e.getMessage());
            result = defValue;
        }
        return result;
    }

    private long getLongProperty(String key, long defValue) {
        long result = 0;
        String propertyValue = env.getProperty(key);
        try {
            if (propertyValue != null) {
                result = Long.parseLong(propertyValue);
            }
        } catch (NumberFormatException e) {
            log.warn(ErrorCode.INVALID_PROPERTY_VALUE.getMessage("long", propertyValue, key), e);
            result = defValue;
        }
        return result;
    }

    private double getDoubleProperty(String key, long defValue) {
        double result = 0;
        String propertyValue = env.getProperty(key);
        try {
            if (propertyValue != null) {
                result = Double.parseDouble(propertyValue);
            }
        } catch (NumberFormatException e) {
            log.warn(ErrorCode.INVALID_PROPERTY_VALUE.getMessage("double", propertyValue, key), e);
            result = defValue;
        }
        return result;
    }

    public int getTimeout() {
        return timeout;
    }

    public int getHistoryItemCount() {
        return historyItemCount;
    }
}
