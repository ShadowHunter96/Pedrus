package cz.bbn.cerberus.email.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.grid.ColumnTextAlign;
import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.AppGridDataVariables;
import cz.bbn.cerberus.commons.AppGridStringVariables;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteAction;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.email.EmailComponentOperations;
import cz.bbn.cerberus.email.dto.EmailSimpleDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class EmailGridComponent extends AppInfiniteGrid<EmailSimpleDto> {

    private final EmailComponentOperations componentOperations;

    public EmailGridComponent(DeleteAction deleteAction, AppEnv appEnv, ItemsAction<EmailSimpleDto> itemsAction,
                              EmailComponentOperations emailComponentOperations) {
        super(deleteAction, appEnv, itemsAction);
        this.componentOperations = emailComponentOperations;
        initGrid();
    }

    private void initGrid() {
        addColumn(EmailSimpleDto::getSubject).setHeader(Transl.get("Email subject")).setSortable(true)
                .setKey("subject");
        addColumn(EmailSimpleDto::getSender).setHeader(Transl.get("Sender"))
                .setSortable(true).setKey("sender");
        addColumn(emailSimpleDto -> AppUtils.formatDateTime(emailSimpleDto.getDateAndTime(), true))
                .setHeader(Transl.get("Sent"))
                .setSortable(true)
                .setKey("date");
        addColumn(EmailSimpleDto::getNoOfAttachments).setHeader(Transl.get("No. of attachments")).setSortable(true)
                .setKey("noOfAttachments");
        addColumn(new ComponentRenderer<>(this::getEntityType)).setHeader(Transl.get("Entity type"))
                .setSortable(true).setKey("entityType");
        addColumn(new ComponentRenderer<>(this::getEntityName)).setHeader(Transl.get("Entity name")).setSortable(true)
                .setKey("Entity name");
        setColumnReorderingAllowed(true);
        setMultiSort(true);

        addColumn(new ComponentRenderer<>(this::getGridButtons))
                .setHeader(VaadinComponents.headerSpan(Transl.get("Actions")))
                .setWidth(VaadinValues.COLUMN_ACTION_SIZE_MEDIUM)
                .setFlexGrow(0).setTextAlign(ColumnTextAlign.CENTER);

        addItemDoubleClickListener(event -> openDetailDialog(event.getItem().getId()));

        loadData();
    }

    private Span getEntityType(EmailSimpleDto item) {
        DomainEnum domainEnum = DomainEnum.getDomainByValue(item.getEntityType());
        return new Span(domainEnum.getTranslatedName());
    }

    private Span getEntityName(EmailSimpleDto item) {
        return new Span(componentOperations.getObjectName(item));
    }

    private HorizontalLayout getGridButtons(EmailSimpleDto item) {
        AppGridStringVariables stringVariables = new AppGridStringVariables("Open email",
                "Delete email {0}? Email will be permanently deleted.",
                "Delete email");
        AppGridDataVariables dataVariables = new AppGridDataVariables(Permission.EMAIL_EDIT, Permission.EMAIL_DELETE,
                String.valueOf(item.getId()), item.getSubject(), null, false);


        HorizontalLayout buttons = new HorizontalLayout();
        buttons.setClassName("buttons-layout");

        Button view = VaadinComponents.getViewButton();
        view.getElement().setProperty(TextValues.TITLE, Transl.get("View"));
        view.addClickListener(e -> openDetailDialog(item.getId()));
        buttons.add(view);

        if (SecurityUtils.hasCustomPermission(item.getEntityType(), item.getEntityId(),
                componentOperations.getDeletePermByDomain(item.getEntityType()))) {
            Button delete = VaadinComponents.getDeleteButton(false);
            AppUtils.addRfClassToGridButton(delete, String.valueOf(item.getId()));
            delete.addClickListener(buttonClickEvent ->
                    getDeleteAction(dataVariables, stringVariables, this, false));

            delete.getElement().setProperty(TextValues.TITLE, Transl.get("Delete"));
            buttons.add(delete);
        }
        return buttons;
    }

    private void openDetailDialog(Long id) {
        try {
            new EmailDetailDialog(componentOperations.getEmailDto(id), componentOperations).open();
        } catch (SystemException e) {
            log.error(e.getMessage(), e);
            ErrorNotification.show(e.getMessage(), getAppEnv());
        }
    }
}
