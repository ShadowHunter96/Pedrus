package cz.bbn.cerberus.commons.security;

import com.vaadin.flow.spring.security.VaadinWebSecurityConfigurerAdapter;
import cz.bbn.cerberus.commons.AppUrlValues;
import cz.bbn.cerberus.exception.ui.GlobalExceptionView;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityCustomizer;
import org.springframework.security.oauth2.client.oidc.userinfo.OidcUserRequest;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserService;
import org.springframework.security.oauth2.core.oidc.user.OidcUser;


@Configuration
@EnableWebSecurity
@DependsOn("appEnv")
@ConditionalOnExpression("'${spring.cloud.azure.active-directory.enabled}' == 'true'")
public class SecurityConfigAzure extends VaadinWebSecurityConfigurerAdapter {


    private final OAuth2UserService<OidcUserRequest, OidcUser> oidcUserService;

    public SecurityConfigAzure(OAuth2UserService<OidcUserRequest, OidcUser> oidcUserService) {
        this.oidcUserService = oidcUserService;
    }

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        super.configure(http);
        http.oauth2Login().userInfoEndpoint().oidcUserService(oidcUserService);
    }

    /**
     * Allows access to static resources, bypassing Spring security.
     */
    @Override
    public void configure(WebSecurity web) throws Exception {
        web.ignoring().antMatchers("/avizo-public/**",
                // Vaadin Flow static resources
                "/ui/VAADIN/**", "/VAADIN/**",

                // the robots exclusion standard
                "/robots.txt",

                // web application manifest
                "/manifest.webmanifest", "/sw.js", "/offline-page.html", "/ui/manifest.webmanifest", "/ui/sw.js",

                "/ui/offline-page.html",

                // icons and images
                "/icons/**", "/images/**", "/ui/icons/**", "/ui/images/**", "/icons/**", "/vaadinServlet/**",
                "/cas/**", "/img/**", "/image-png/**",

                // (development mode) static resources
                "/ui/frontend/**", "/frontend/**",

                // (development mode) webjars
                "/ui/webjars/**", "/webjars/**",

                // (production mode) static resources
                "/ui/frontend-es5/**", "/ui/frontend-es6/**", "/frontend-es5/**", "/frontend-es6/**",

                "/error",
                "/" + GlobalExceptionView.ROUTE,
                "/admins/monitoring/**",
                "/api/**",
                AppUrlValues.ACTUATOR);
        super.configure(web);
    }

    @Bean
    public WebSecurityCustomizer webSecurityCustomizer() {
        return SecurityUtils::securityConfigure;
    }
}
