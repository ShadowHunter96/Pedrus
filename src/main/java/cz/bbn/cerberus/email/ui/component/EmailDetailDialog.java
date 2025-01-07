package cz.bbn.cerberus.email.ui.component;

import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.html.Div;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.server.StreamResource;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.email.EmailComponentOperations;
import cz.bbn.cerberus.email.dto.EmailDto;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.simplejavamail.outlookmessageparser.OutlookMessageParser;
import org.simplejavamail.outlookmessageparser.model.OutlookFileAttachment;
import org.simplejavamail.outlookmessageparser.model.OutlookMessage;
import org.vaadin.olli.FileDownloadWrapper;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

@Slf4j
public class EmailDetailDialog extends AppDialog {

    private final EmailDto dto;
    private final EmailComponentOperations componentOperations;

    public EmailDetailDialog(EmailDto dto, EmailComponentOperations componentOperations) {
        this.dto = dto;
        this.componentOperations = componentOperations;
        initDialog();
    }

    private void initDialog() {

        setTitle(Transl.get("Email"));

        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setMargin(false);
        verticalLayout.setSizeFull();

        FormLayout itemInfoFormLayout = new FormLayout();

        TextField entityName = new TextField(Transl.get("Entity name"));
        entityName.setValue(dto.getEntityId());
        entityName.setReadOnly(true);
        itemInfoFormLayout.add(entityName);

        TextField domain = new TextField(Transl.get("Entity type"));
        domain.setValue(DomainEnum.getDomainByValue(dto.getEntityType()).getTranslatedName());
        domain.setReadOnly(true);
        itemInfoFormLayout.add(domain);

        TextField sender = new TextField(Transl.get("Sender"));
        sender.setValue(dto.getSender());
        sender.setReadOnly(true);
        itemInfoFormLayout.add(sender);

        TextField date = new TextField(Transl.get("Sent"));
        date.setValue(AppUtils.formatDateTime(dto.getDateAndTime(), true));
        date.setReadOnly(true);
        itemInfoFormLayout.add(date);

        TextField noOfAttachments = new TextField(Transl.get("No. of attachments"));
        noOfAttachments.setValue(String.valueOf(dto.getNoOfAttachments()));
        noOfAttachments.setReadOnly(true);
        itemInfoFormLayout.add(noOfAttachments);

        verticalLayout.add(itemInfoFormLayout);

        TextField subject = new TextField(Transl.get("Email subject"));
        if (dto.getSubject() != null) {
            subject.setValue(dto.getSubject());
        }
        subject.setReadOnly(true);
        subject.setWidthFull();
        verticalLayout.add(subject);

        OutlookMessageParser parser = new OutlookMessageParser();
        OutlookMessage message;
        try {
            message = parser.parseMsg(dto.getFile());
        } catch (IOException e) {
            log.error(e.getMessage(), e);
            message = new OutlookMessage();
            ErrorNotification.show(Transl.get("Error parsing email"), componentOperations.egtAppEnv());
        }

        Label bodyLabel = new Label(Transl.get("Email body"));
        verticalLayout.add(bodyLabel);

        Div bodyDiv = new Div();
        String messageHtmlBody;
        if (message.getBodyRTF().contains("ansicpg1252")) {
            try {
                byte[] bytes = message.getConvertedBodyHTML().getBytes("Windows-1252");
                messageHtmlBody = new String(bytes, "Windows-1250");
            } catch (UnsupportedEncodingException e) {
                log.error("Unsupported encoding", e);
                messageHtmlBody = message.getConvertedBodyHTML();
            }
        } else {
            messageHtmlBody = message.getConvertedBodyHTML();
        }
        bodyDiv.getElement().setProperty("innerHTML", messageHtmlBody);
        bodyDiv.setSizeFull();
        verticalLayout.add(bodyDiv);

        if (dto.getNoOfAttachments() > 0) {
            Label attachments = new Label(Transl.get("Attachments"));
            verticalLayout.add(attachments);
            HorizontalLayout attachmentsLayout = new HorizontalLayout();
            for (OutlookFileAttachment attachment : message.fetchTrueAttachments()) {
                HorizontalLayout attachmentDiv = new HorizontalLayout();
                Icon icon = VaadinIcon.DOWNLOAD.create();
                attachmentDiv.add(icon);
                Label attachmentLabel = new Label(attachment.getFilename());
                attachmentDiv.add(attachmentLabel);

                StreamResource resource = new StreamResource(
                        attachment.getFilename(),
                        () -> new ByteArrayInputStream(attachment.getData()));

                FileDownloadWrapper buttonWrapper = new FileDownloadWrapper(resource);
                buttonWrapper.wrapComponent(attachmentDiv);
                attachmentsLayout.add(buttonWrapper);

            }
            verticalLayout.add(attachmentsLayout);
        }

        this.setContent(verticalLayout);

        addCloseButton();
    }
}
