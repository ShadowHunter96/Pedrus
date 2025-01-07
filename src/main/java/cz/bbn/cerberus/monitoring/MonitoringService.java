package cz.bbn.cerberus.monitoring;

import cz.bbn.cerberus.changelog.ChangelogService;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.io.PrintWriter;
import java.io.StringWriter;

@RestController
@RequestMapping("admins/monitoring")
@Slf4j
public class MonitoringService {

    private final AppEnv appEnv;
    private final ChangelogService changelogService;

    private Exception exception;

    public MonitoringService(AppEnv appEnv, ChangelogService changelogService) {
        this.appEnv = appEnv;
        this.changelogService = changelogService;
    }

    @GetMapping("/last-error")
    public ResponseEntity<String> getLastError() {
        if (exception == null) {
            return new ResponseEntity<>("Application is running correctly", HttpStatus.OK);
        }
        if (appEnv.isMonitoringShowStackTrace()) {
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            exception.printStackTrace(pw);
            String stackTrace = sw.toString();
            return new ResponseEntity<>("Application error!".concat("<br>").concat(
                    stackTrace.replace(System.getProperty("line.separator"), "<br/>\n")),
                    HttpStatus.INTERNAL_SERVER_ERROR);
        }
        return new ResponseEntity<>("Application error!", HttpStatus.INTERNAL_SERVER_ERROR);
    }

    public void setException(Exception exception) {
        this.exception = exception;
    }

    @GetMapping("/db-connected")
    public ResponseEntity<String> getDbConnected() {
        try {
            changelogService.findAllVersionList();
            return new ResponseEntity<>("DB connected!", HttpStatus.OK);
        } catch (Exception e) {
            return new ResponseEntity<>("DB connection error!", HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }
}
