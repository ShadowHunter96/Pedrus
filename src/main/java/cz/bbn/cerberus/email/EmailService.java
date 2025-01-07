package cz.bbn.cerberus.email;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.email.dto.EmailDto;
import cz.bbn.cerberus.email.dto.EmailFilterDto;
import cz.bbn.cerberus.email.dto.EmailSimpleDto;
import cz.bbn.cerberus.email.factory.EmailFactory;
import cz.bbn.cerberus.email.persistence.EmailDao;
import cz.bbn.cerberus.email.persistence.entity.EmailEntity;
import cz.bbn.cerberus.email.persistence.repository.EmailRepository;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.extern.slf4j.Slf4j;
import org.simplejavamail.outlookmessageparser.OutlookMessageParser;
import org.simplejavamail.outlookmessageparser.model.OutlookMessage;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.time.LocalDateTime;

@Service
@Slf4j
public class EmailService {

    private final EmailRepository emailRepository;
    private final EmailDao emailDao;
    private final AppLogService appLogService;
    private final ListService listService;

    public EmailService(EmailRepository emailRepository, EmailDao emailDao, AppLogService appLogService, ListService listService) {
        this.emailRepository = emailRepository;
        this.emailDao = emailDao;
        this.appLogService = appLogService;
        this.listService = listService;
    }

    public Page<EmailSimpleDto> findEmailDtoPage(EmailFilterDto filter) {
        return emailDao.findEmailDtoPage(filter);
    }

    public void deleteEmail(Long emailId) throws SystemException {
        if (!emailRepository.existsById(emailId)) {
            throw new SystemException(ErrorCode.EMAIL_NOT_EXISTS, emailId);
        }
        emailRepository.deleteById(emailId);
        appLogService.logDelete(String.valueOf(emailId), DomainEnum.EMAIL_DOMAIN_NAME.getValue());

    }

    public Long saveEmail(EmailDto dto) throws SystemException {
        try {
            EmailEntity entity = new EmailEntity();
            EmailFactory.fillEntity(entity, dto);
            entity = emailRepository.save(entity);
            EmailSimpleDto emailSimpleDto = EmailFactory.simpleFromRegularDto(dto);
            appLogService.logInsert(emailSimpleDto, DomainEnum.EMAIL_DOMAIN_NAME.getValue());
            return entity.getId();
        } catch (IOException e) {
            log.error(e.getMessage(), e);
            throw new SystemException(ErrorCode.SAVE_EMAIL_ERROR);
        }
    }

    public EmailDto getEmailDto(Long id) throws SystemException {
        if (emailRepository.existsById(id)) {
            return EmailFactory.fromEntity(emailRepository.findById(id).get());
        }
        throw new SystemException(ErrorCode.EMAIL_NOT_EXISTS, id);
    }

    public void parseAndSaveHtmlEmail(MultipartFile file, String entityId,
                                      String entityType, String login,
                                      String subject,
                                      String sender,
                                      String customer,
                                      int attachmentCount,
                                      LocalDateTime localDateTime) throws SystemException, IOException {


        EmailDto emailDto = new EmailDto();
        emailDto.setEntityId(entityId);
        emailDto.setFile(file.getInputStream());
        emailDto.setEntityType(entityType.substring(0, 1).toUpperCase() + entityType.substring(1).toLowerCase());
        emailDto.setCustomer(customer);
        emailDto.setSubject(subject);
        emailDto.setSender(sender);
        emailDto.setNoOfAttachments(attachmentCount);
        emailDto.setDateAndTime(localDateTime);
        saveEmail(emailDto);
        UserDto userDto = listService.getUserDtoList().stream()
                .filter(actualUserDto -> actualUserDto.getLogin().equals(login)).findFirst().orElseThrow(() -> new SystemException(ErrorCode.USER_NOT_EXISTS, login));


        appLogService.log("Call rest api", "Call method save email", userDto.getId());
    }
}
