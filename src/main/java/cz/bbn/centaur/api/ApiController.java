package cz.bbn.cerberus.api;

import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.email.EmailService;
import cz.bbn.cerberus.subject.SubjectService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Objects;

@RestController
@RequestMapping("api")
@Slf4j
public class ApiController {

    private final SubjectService subjectService;
    private final ApiService apiService;
    private final AppEnv appEnv;
    private final EmailService emailService;

    public ApiController(SubjectService subjectService, ApiService apiService, AppEnv appEnv, EmailService emailService) {
        this.subjectService = subjectService;
        this.apiService = apiService;
        this.appEnv = appEnv;
        this.emailService = emailService;
    }

    @GetMapping("/update-subjects/{count}")
    public ResponseEntity<?> updateSubjects(@PathVariable int count,
                                            @RequestHeader(HttpHeaders.AUTHORIZATION) String apiKey) {
        return doApiAction(
                () -> {
                    subjectService.updateChungFromAres(count);
                    return new ResponseEntity<>(HttpStatus.OK);
                }, apiKey);

    }

    @PostMapping("/save-email")
    public ResponseEntity<?> saveEmail(@RequestHeader(HttpHeaders.AUTHORIZATION) String apiKey,
                                       @RequestParam("file") MultipartFile file,
                                       @RequestParam("attachmentCount") Integer attachmentCount,
                                       @RequestParam("entityId") String entityId,
                                       @RequestParam("entityType") String entityType,
                                       @RequestParam("login") String login,
                                       @RequestParam("subject") String subject,
                                       @RequestParam("sender") String sender,
                                       @RequestParam("customer") String customer,
                                       @RequestParam("date") String date) {
        return doApiAction(() -> {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss");
            emailService.parseAndSaveHtmlEmail(file, entityId, entityType,
                    login, subject, sender, customer, attachmentCount, LocalDateTime.parse(date, formatter));
            return new ResponseEntity<>(HttpStatus.OK);
        }, apiKey);

    }

    @GetMapping("/entity-type-list")
    public ResponseEntity<?> getEntityTypeList(@RequestHeader(HttpHeaders.AUTHORIZATION) String apiKey) {
        return doApiAction(() -> new ResponseEntity<>(ObjectType.getEntityTypeListForApi(), HttpStatus.OK), apiKey);
    }

    @GetMapping("/customer-list/{login}")
    public ResponseEntity<?> getCustomerList(@RequestHeader(HttpHeaders.AUTHORIZATION) String apiKey,
                                             @PathVariable String login) {
        return doApiAction(() -> new ResponseEntity<>(apiService.getCustomerList(login), HttpStatus.OK), apiKey);
    }

    @GetMapping("/entity-list/{login}/{entityType}")
    public ResponseEntity<?> getEntityList(@RequestHeader(HttpHeaders.AUTHORIZATION) String apiKey,
                                           @PathVariable String login, @PathVariable String entityType) {
        return doApiAction(() -> new ResponseEntity<>(apiService.getItemDtoListByType(login, entityType), HttpStatus.OK), apiKey);
    }

    private ResponseEntity<?> doApiAction(ApiAction<?> apiAction, String apiKey) {
        if (Objects.equals(appEnv.getApiKey(), apiKey)) {
            try {
                return apiAction.doAction();
            } catch (Exception e) {
                log.error("Exception", e);
                return new ResponseEntity<>(e.getMessage(), HttpStatus.EXPECTATION_FAILED);
            }
        } else {
            return new ResponseEntity<>(null, HttpStatus.FORBIDDEN);
        }
    }

}
