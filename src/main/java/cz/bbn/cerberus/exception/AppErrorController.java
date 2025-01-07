package cz.bbn.cerberus.exception;

import cz.bbn.cerberus.exception.ui.GlobalExceptionView;
import org.springframework.boot.web.servlet.error.ErrorController;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.servlet.RequestDispatcher;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

@RestController
public class AppErrorController implements ErrorController {

    @RequestMapping("/error")
    public String handleError(HttpServletResponse response, HttpServletRequest request) throws IOException {
        Exception e = (Exception) request.getAttribute(RequestDispatcher.ERROR_EXCEPTION);
        if (e != null && e.getCause().getMessage().startsWith("Login error")) {
            response.sendRedirect("/login".concat("?error"));
            request.getSession().setAttribute("message", e.getCause().getMessage());
        } else {
            response.sendRedirect("/" + GlobalExceptionView.ROUTE);
        }
        return "error";
    }
}