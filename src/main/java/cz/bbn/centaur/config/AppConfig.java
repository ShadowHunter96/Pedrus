package cz.bbn.cerberus.config;

import com.vaadin.flow.server.VaadinServlet;
import com.vaadin.flow.spring.annotation.EnableVaadin;
import org.springframework.boot.web.servlet.ServletRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

@Configuration
@EnableVaadin
@ComponentScan(basePackages = {"cz.solutia.cerberus"})
@EnableScheduling
public class AppConfig {

    @Bean
    public ServletRegistrationBean<VaadinServlet> frontendServletBean() {
        ServletRegistrationBean<VaadinServlet> bean = new ServletRegistrationBean<>(new VaadinServlet() {
            @Override
            protected void service(HttpServletRequest req, HttpServletResponse resp) throws IOException {
                if (!serveStaticOrWebJarRequest(req, resp)) {
                    resp.sendError(404);
                }
            }
        }, "/frontend/*", "/img/*");
        bean.setLoadOnStartup(1);
        return bean;
    }
}
