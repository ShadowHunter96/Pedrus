package cz.bbn.cerberus.dsmessage.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.html.Label;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.server.StreamResource;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.interfaces.GetItemAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.dsmessage.dto.DsMessageAttachementDto;
import cz.bbn.cerberus.dsmessage.dto.DsMessageDto;
import cz.bbn.cerberus.dsmessage.ui.DsMessageView;
import cz.bbn.cerberus.translation.Transl;
import org.vaadin.olli.FileDownloadWrapper;

import java.io.InputStream;

public class DsMessageDetailComponent extends AppDetailCardComponent<DsMessageDto> {

    private final GetItemAction<InputStream> getItemAction;

    public DsMessageDetailComponent(DsMessageDto dto, SaveAction<DsMessageDto> saveAction,
                                    boolean showSubmitButton, AppEnv appEnv, GetItemAction<InputStream> getItemAction,
                                    EntityNewComponentOperation entityNewComponentOperation) {
        super(dto, saveAction, showSubmitButton, appEnv, entityNewComponentOperation);
        this.getItemAction = getItemAction;
        initComponent();
    }

    @Override
    protected void initComponent() {
        String heading = Transl.get("DS message")
                .concat(" - ")
                .concat(String.valueOf(getDto().getId()));
        setHeading(heading);
        this.addBackButton(DsMessageView.ROUTE);
        this.setId(RobotFrameworkVariables.DS_MESSAGE_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        FormLayout formLayout = new FormLayout();
        TextField id = new TextField(Transl.get("DS id"));
        id.setValue(String.valueOf(getDto().getId()));
        id.setReadOnly(true);
        formLayout.add(id);

        TextField recipientId = new TextField(Transl.get("Recipient id"));
        recipientId.setReadOnly(true);
        recipientId.setValue(getDto().getRecipientId());
        formLayout.add(recipientId);

        TextField senderId = new TextField(Transl.get("Sender id"));
        senderId.setReadOnly(true);
        senderId.setValue(getDto().getSenderId());
        formLayout.add(senderId);

        TextField senderName = new TextField(Transl.get("Sender name"));
        senderName.setReadOnly(true);
        senderName.setValue(getDto().getSenderName());
        formLayout.add(senderName);

        TextField senderAddress = new TextField(Transl.get("Sender address"));
        senderAddress.setReadOnly(true);
        senderAddress.setValue(getDto().getSenderAddress());
        formLayout.add(senderAddress);

        TextField type = new TextField(Transl.get("Type"));
        type.setReadOnly(true);
        type.setValue(Transl.get(getDto().getType().name()));
        formLayout.add(type);

        TextField deliveryDate = new TextField(Transl.get("Delivery time"));
        deliveryDate.setReadOnly(true);
        deliveryDate.setValue(AppUtils.formatDateTime(getDto().getDeliveryTime(), true));
        formLayout.add(deliveryDate);

        TextField acceptanceTime = new TextField(Transl.get("Acceptance time"));
        acceptanceTime.setReadOnly(true);
        acceptanceTime.setValue(AppUtils.formatDateTime(getDto().getAcceptanceTime(), true));
        formLayout.add(acceptanceTime);

        TextField createdInAppTime = new TextField(Transl.get("Created in application"));
        createdInAppTime.setReadOnly(true);
        createdInAppTime.setValue(AppUtils.formatDateTime(getDto().getCreatedInAppTime(), true));
        formLayout.add(createdInAppTime);

        HorizontalLayout viewedHorizontalLayout = new HorizontalLayout(
                new HorizontalLayout(new Label(Transl.get("Viewed")), getIcon(getDto().getViewed())));
        viewedHorizontalLayout.setAlignItems(Alignment.CENTER);
        viewedHorizontalLayout.setMinHeight("3em");
        formLayout.add(viewedHorizontalLayout);

        HorizontalLayout deletedHorizontalLayout = new HorizontalLayout(
                new HorizontalLayout(new Label(Transl.get("Deleted")), getIcon(getDto().getDeleted())));
        deletedHorizontalLayout.setAlignItems(Alignment.CENTER);
        deletedHorizontalLayout.setMinHeight("3em");
        formLayout.add(deletedHorizontalLayout);

        getBinder().setBean(getDto());

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        HorizontalLayout horizontalLayout = new HorizontalLayout();
        horizontalLayout.setWidthFull();
        TextArea subject = new TextArea("Subject");
        subject.setWidthFull();
        subject.setHeight(CssVariables.DEFAULT_FIELD_HEIGHT.getValue());
        subject.setReadOnly(true);
        subject.setValue(getDto().getSubject());
        horizontalLayout.add(subject);

        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.add(formLayout, horizontalLayout);

        TextField attachementSize = new TextField(Transl.get("Attachement size"));
        attachementSize.setValue(String.valueOf(getDto().getAttachementSize()));
        attachementSize.setReadOnly(true);
        verticalLayout.add(attachementSize);

        for (DsMessageAttachementDto dsMessageAttachementDto : getDto().getDsMessageAttachementDtoList()) {
            Button downloadButton = VaadinComponents.getButton(VaadinIcon.DOWNLOAD.create());
            downloadButton.getElement().setProperty(TextValues.TITLE, Transl.get("Download"));
            StreamResource resource = new StreamResource(
                    dsMessageAttachementDto.getName(),
                    () -> getItemAction.getItem(String.valueOf(dsMessageAttachementDto.getId())));

            HorizontalLayout oneAttachementLayout = new HorizontalLayout();
            oneAttachementLayout.setAlignItems(FlexComponent.Alignment.CENTER);

            FileDownloadWrapper buttonWrapper = new FileDownloadWrapper(resource);
            buttonWrapper.wrapComponent(downloadButton);
            oneAttachementLayout.add(buttonWrapper, new Label(dsMessageAttachementDto.getName()));
            verticalLayout.add(oneAttachementLayout);
        }

        this.add(verticalLayout);

    }

    private Span getIcon(boolean value) {
        return new Span(VaadinComponents.getBooleanIcon(value));
    }

}
