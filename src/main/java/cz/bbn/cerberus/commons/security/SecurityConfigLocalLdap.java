package cz.bbn.cerberus.commons.security;

import com.vaadin.flow.spring.security.VaadinWebSecurityConfigurerAdapter;
import cz.bbn.cerberus.commons.AppUrlValues;
import cz.bbn.cerberus.exception.ui.GlobalExceptionView;
import cz.bbn.cerberus.login.ui.LoginView;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.configuration.AuthenticationConfiguration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityCustomizer;
import org.springframework.security.web.authentication.logout.SecurityContextLogoutHandler;


@Configuration
@EnableWebSecurity
@DependsOn("appEnv")
@ConditionalOnExpression("'${spring.cloud.azure.active-directory.enabled}' == 'false'")
public class SecurityConfigLocalLdap extends VaadinWebSecurityConfigurerAdapter {

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http.rememberMe().alwaysRemember(false);

        http.authorizeRequests().antMatchers("/VAADIN/**").permitAll()
                .and().authorizeRequests().antMatchers("/services/**").permitAll()
                .and().requestCache().requestCache(new SecurityRequestCache())
                .and().formLogin().loginProcessingUrl(AppUrlValues.LOGIN_PROCESSING_URL)
                .and().formLogin().defaultSuccessUrl(AppUrlValues.LOGOUT_SUCCESS_URL)
                .and().formLogin().failureUrl(AppUrlValues.LOGIN_FAILURE_URL);

        super.configure(http);

        setLoginView(http, LoginView.class);
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
                AppUrlValues.ACTUATOR);
        super.configure(web);
    }

    @Bean
    public WebSecurityCustomizer webSecurityCustomizer() {
        return SecurityUtils::securityConfigure;
    }

    @Bean
    public AuthenticationManager authenticationManager(AuthenticationConfiguration authenticationConfiguration)
            throws Exception {
        return authenticationConfiguration.getAuthenticationManager();
    }

    @Bean
    public SecurityContextLogoutHandler securityContextLogoutHandler() {
        return new SecurityContextLogoutHandler();
    }

}