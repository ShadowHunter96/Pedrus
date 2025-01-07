package cz.bbn.cerberus.email.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.component.upload.Upload;
import com.vaadin.flow.component.upload.receivers.MemoryBuffer;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.email.EmailComponentOperations;
import cz.bbn.cerberus.email.dto.EmailDto;
import cz.bbn.cerberus.email.dto.EmailImportDto;
import cz.bbn.cerberus.email.dto.EmailSimpleDto;
import cz.bbn.cerberus.email.dto.SimpleItemDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.jline.utils.Log;
import org.simplejavamail.outlookmessageparser.OutlookMessageParser;
import org.simplejavamail.outlookmessageparser.model.OutlookMessage;

import java.io.IOException;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.List;

@Slf4j
public class EmailImportDialog extends AppDialog {

    private final EmailComponentOperations emailComponentOperations;
    private final AppInfiniteGrid<EmailSimpleDto> grid;
    private final AppEnv appEnv;
    private final EmailImportDto emailImportDto = new EmailImportDto();

    public EmailImportDialog(EmailComponentOperations emailComponentOperations, AppInfiniteGrid<EmailSimpleDto> grid,
                             AppEnv appEnv) {
        this.emailComponentOperations = emailComponentOperations;
        this.grid = grid;
        this.appEnv = appEnv;
        initDialog();
    }

    private void initDialog() {

        setTitle(Transl.get("Import new email"));

        Binder<EmailImportDto> binder = new Binder<>();
        binder.setBean(emailImportDto);

        VerticalLayout content = new VerticalLayout();
        content.setSizeFull();
        content.setMargin(false);

        FormLayout formLayout = new FormLayout();
        formLayout.setSizeFull();

        ComboBox<SubjectDto> customerCombobox = new ComboBox<>(Transl.get("Customer"));
        List<SubjectDto> customerList = emailComponentOperations.getCustomerList();
        customerCombobox.setItems(customerList);
        customerCombobox.setItemLabelGenerator(SubjectDto::getName);
        binder.forField(customerCombobox).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(EmailImportDto::getCustomer, EmailImportDto::setCustomer);

        ComboBox<DomainEnum> domainEnumComboBox = new ComboBox<>(Transl.get("Entity type"));
        domainEnumComboBox.setItems(emailComponentOperations.getSubjectDomainEnum());
        domainEnumComboBox.setItemLabelGenerator(DomainEnum::getTranslatedName);
        binder.forField(domainEnumComboBox).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(EmailImportDto::getDomain, EmailImportDto::setDomain);

        ComboBox<SimpleItemDto> simpleItemComboBox = new ComboBox<>(Transl.get("Entity"));
        simpleItemComboBox.setItemLabelGenerator(SimpleItemDto::getName);
        binder.forField(simpleItemComboBox).bind(EmailImportDto::getPickedEntity, EmailImportDto::setPickedEntity);
        simpleItemComboBox.setReadOnly(true);

        customerCombobox.addValueChangeListener(e -> {
            simpleItemComboBox.setValue(null);
            if (e.getValue() != null && domainEnumComboBox.getValue() != null
                    && domainEnumComboBox.getValue() != DomainEnum.SUBJECT_DOMAIN_NAME) {
                simpleItemComboBox.setItems(
                        emailComponentOperations.getSimpleItems(domainEnumComboBox.getValue(), e.getValue().getId()));
                simpleItemComboBox.setReadOnly(false);
                simpleItemComboBox.setRequired(true);
            } else {
                simpleItemComboBox.setItems(new ArrayList<>());
                simpleItemComboBox.setReadOnly(true);
                simpleItemComboBox.setRequired(false);
            }
        });

        domainEnumComboBox.addValueChangeListener(e -> {
            simpleItemComboBox.setValue(null);
            if (e.getValue() != null && customerCombobox.getValue() != null
                    && e.getValue() != DomainEnum.SUBJECT_DOMAIN_NAME) {
                simpleItemComboBox.setItems(
                        emailComponentOperations.getSimpleItems(e.getValue(), customerCombobox.getValue().getId()));
                simpleItemComboBox.setReadOnly(false);
                simpleItemComboBox.setRequired(true);
            } else {
                simpleItemComboBox.setItems(new ArrayList<>());
                simpleItemComboBox.setReadOnly(true);
                simpleItemComboBox.setRequired(false);
            }
        });

        formLayout.add(customerCombobox, domainEnumComboBox, simpleItemComboBox);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
        content.add(formLayout);

        TextField subject = new TextField(Transl.get("Email subject"));
        subject.setWidthFull();
        subject.setReadOnly(true);
        content.add(subject);

        MemoryBuffer memoryBuffer = new MemoryBuffer();
        Upload upload = new Upload(memoryBuffer);
        upload.setDropAllowed(true);
        int maxFileSizeInBytes = 20 * 1024 * 1024; // 20MB
        upload.setMaxFileSize(maxFileSizeInBytes);
        upload.setAcceptedFileTypes(".msg", ".MSG");
        upload.setSizeFull();

        upload.addFileRejectedListener(event -> {
            String errorMessage = event.getErrorMessage();
            ErrorNotification.show(Transl.get(errorMessage), appEnv);
        });

        upload.addSucceededListener(event -> processMsgEmail(memoryBuffer, subject));

        content.add(upload);

        setContent(content);

        Button submit = VaadinComponents.getSubmitButton();

        submit.addClickListener(e -> {
            submit.setEnabled(false);
            if (binder.validate().isOk() && emailImportDto.getOutlookMessage() != null
                    && (domainEnumComboBox.getValue() == DomainEnum.SUBJECT_DOMAIN_NAME
                    || simpleItemComboBox.getValue() != null)) {
                EmailDto emailDto = new EmailDto();
                emailDto.setCustomer(customerCombobox.getValue().getId());
                if (simpleItemComboBox.getValue() == null) {
                    emailDto.setEntityId(customerCombobox.getValue().getId());
                    emailDto.setEntityType(DomainEnum.SUBJECT_DOMAIN_NAME.getValue());
                } else {
                    emailDto.setEntityId(simpleItemComboBox.getValue().getId());
                    emailDto.setEntityType(domainEnumComboBox.getValue().getValue());
                }
                OutlookMessage message = emailImportDto.getOutlookMessage();
                if (message.getSubject() != null) {
                    emailDto.setSubject(message.getSubject());
                } else {
                    emailDto.setSubject("");
                }
                if (message.getBodyText() != null) {
                    emailDto.setBody(message.getBodyText());
                } else {
                    emailDto.setBody("");
                }
                emailDto.setSender(message.getFromEmail());
                emailDto.setDateAndTime(message.getDate().toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime());
                emailDto.setNoOfAttachments(message.fetchTrueAttachments().size());
                emailDto.setFile(emailImportDto.getMemoryBuffer().getInputStream());
                emailComponentOperations.getSaveAction(grid, this).saveItem(emailDto, null);
            }

            if (emailImportDto.getOutlookMessage() == null) {
                ErrorNotification.show(Transl.get(TextValues.FILE_CANNOT_BE_EMPTY), appEnv);
            }
            submit.setEnabled(true);
        });

        addCloseButton();
        addSubmitButton(submit);
    }

    private void processMsgEmail(MemoryBuffer memoryBuffer, TextField subject) {
        OutlookMessageParser parser = new OutlookMessageParser();
        try {
            emailImportDto.setOutlookMessage(parser.parseMsg(memoryBuffer.getInputStream()));
            emailImportDto.setMemoryBuffer(memoryBuffer);
            if (emailImportDto.getOutlookMessage().getSubject() != null) {
                subject.setValue(emailImportDto.getOutlookMessage().getSubject());
            } else {
                subject.setValue("");
            }
        } catch (IOException e) {
            Log.error("Email parse error", e);
            ErrorNotification.show(Transl.get("Error parsing email"), appEnv);
        }

    }
}
