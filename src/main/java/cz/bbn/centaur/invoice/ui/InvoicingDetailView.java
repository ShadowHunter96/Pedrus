package cz.bbn.cerberus.invoice.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.button.ButtonVariant;
import com.vaadin.flow.component.html.H2;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.data.binder.Binder;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.OptionalParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologyComponentOperation;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCard;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ConfirmAction;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.ui.ContractSalesDetailView;
import cz.bbn.cerberus.invoice.InvoiceComponentOperation;
import cz.bbn.cerberus.invoice.InvoicingService;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.invoice.dto.InvoiceState;
import cz.bbn.cerberus.invoice.ui.component.InvoiceComponent;
import cz.bbn.cerberus.invoice.ui.component.InvoiceInvoicedDialog;
import cz.bbn.cerberus.invoice.ui.component.InvoicePayedDialog;
import cz.bbn.cerberus.invoice.ui.component.InvoicingDetailListener;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.task.TaskComponentOperation;
import cz.bbn.cerberus.task.ui.component.TaskSlideTabComponent;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.UserService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.StringUtils;

@Route(value = InvoicingDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.INVOICE_VIEW)
@Slf4j
public class InvoicingDetailView extends AppView implements HasUrlParameter<String>, InvoicingDetailListener {

    public static final String ROUTE = "invoice-detail";

    private final InvoicingService invoicingService;
    private final InvoiceComponentOperation invoiceComponentOperation;
    private final AppEnv appEnv;
    private final TaskComponentOperation taskComponentOperation;
    private final UserService userService;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final AreaTechnologyComponentOperation areaTechnologyComponentOperation;
    private final ListService listService;

    private InvoiceDto dto;
    private InvoiceDto originalDto;
    private boolean readOnly = true;
    private Binder<InvoiceDto> binder;
    private InvoiceComponent invoiceComponent;
    private Button forwardForInvoicing;
    private Button invoiced;
    private Button payed;

    private H2 title;

    public InvoicingDetailView(InvoicingService invoicingService, InvoiceComponentOperation invoiceComponentOperation,
                               AppEnv appEnv, TaskComponentOperation taskComponentOperation, UserService userService,
                               EntityNewComponentOperation entityNewComponentOperation,
                               AreaTechnologyComponentOperation areaTechnologyComponentOperation,
                               ListService listService) {
        this.invoicingService = invoicingService;
        this.invoiceComponentOperation = invoiceComponentOperation;
        this.appEnv = appEnv;
        this.taskComponentOperation = taskComponentOperation;
        this.userService = userService;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.areaTechnologyComponentOperation = areaTechnologyComponentOperation;
        this.listService = listService;
    }

    private void initView() {
        AppCard card = new AppCard(entityNewComponentOperation);
        card.setSizeFull();
        card.setId(RobotFrameworkVariables.INVOICE_VIEW_CARD_ID.getValue());

        refreshTitle(dto.getInvoiceNo());

        card.addToHeader(title);

        invoiceComponent = new InvoiceComponent(dto, invoiceComponentOperation, readOnly,
                userService.findAllowedUserList(),
                appEnv, areaTechnologyComponentOperation.getAreaTechnologySignDtoList(
                ObjectType.CONTRACT, dto.getContractDto().getId()));

        card.add(invoiceComponent);

        this.binder = invoiceComponent.getBinder();

        Button submitButton = VaadinComponents.getSubmitButton();
        submitButton.addThemeVariants(ButtonVariant.LUMO_PRIMARY);
        submitButton.addClassName(RobotFrameworkVariables.SAVE_ITEM_BUTTON_CLASS.getValue());
        submitButton.addClickListener(e -> saveInvoice(false));

        Button backButton = VaadinComponents.getBackButton();
        backButton.addClickListener(e -> UI.getCurrent().navigate(ContractSalesDetailView.ROUTE + "/" +
                invoiceComponent.getBinder().getBean().getContractDto().getId()));

        TaskSlideTabComponent taskSlideTabComponent =
                new TaskSlideTabComponent(taskComponentOperation, appEnv, ObjectType.INVOICE,
                        String.valueOf(dto.getId()), dto.getContractDto().getSubjectDto(), listService);
        card.addEventSlideTab(taskSlideTabComponent);

        card.addNewEntitySlideTab(new NewEntityButtonsComponent.Builder(
                entityNewComponentOperation, EntityNewType.SALES)
                .setSubjectDto(dto.getContractDto()
                        .getSubjectDto())
                .build());

        forwardForInvoicing = VaadinComponents.getButton(
                Transl.get("Forward for invoicing"), VaadinIcon.ANGLE_DOUBLE_DOWN.create());
        forwardForInvoicing.addClickListener(e -> this.forwardForInvoicingAction());

        invoiced = VaadinComponents.getButton(Transl.get("Invoiced"), VaadinIcon.ALIGN_LEFT.create());
        invoiced.addClickListener(e -> new InvoiceInvoicedDialog(this).open());

        payed = VaadinComponents.getButton(Transl.get("Payed"), VaadinIcon.ANGLE_DOUBLE_DOWN.create());
        payed.addClickListener(e -> new InvoicePayedDialog(this).open());

        checkStatusForButtons();

        if (dto != null && dto.getContractDto() != null && dto.getContractDto().getSubjectDto() != null) {
            card.showSubjectLink(dto.getContractDto().getSubjectDto());
        }

        card.addToFooter(backButton, forwardForInvoicing, invoiced, payed, submitButton);

        card.showFooter(true);
        add(card);
    }

    @Override
    public void setParameter(BeforeEvent event, @OptionalParameter String parameter) {
        dto = new InvoiceDto();
        originalDto = new InvoiceDto();
        if (parameter != null) {
            try {
                setParameter(parameter);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }

    private void setParameter(String parameter) throws SystemException {
        InvoiceDto tempDto = invoicingService.findById(Long.parseLong(parameter));
        if (SecurityUtils.hasCustomPermission(
                DomainEnum.INVOICE_DOMAIN_NAME.getValue(),
                tempDto.getContractDto().getId(),
                Permission.INVOICE_VIEW.name(), parameter)) {
            dto = tempDto;
            originalDto = SerializationUtils.clone(tempDto);
            refreshBreadcrumbText(StringUtils.isEmpty(dto.getInvoiceNo()) ? AppUtils.formatDate(dto.getInvoicingDate()) : dto.getInvoiceNo());
            if (SecurityUtils.hasCustomPermission(
                    DomainEnum.INVOICE_DOMAIN_NAME.getValue(),
                    tempDto.getContractDto().getId(),
                    Permission.INVOICE_EDIT.name(), parameter)) {
                readOnly = false;
            }
            initView();
        } else {
            ErrorNotification.show(ErrorCode.VIEW_PERMISSION_MISSING.getError(), appEnv);
            UI.getCurrent().access(
                    () -> UI.getCurrent().getPage().fetchCurrentURL(e -> UI.getCurrent().navigate(e.getPath()))
            );
        }
    }

    private void saveInvoice(boolean stay) {
        if (binder.validate().isOk()) {
            try {
                invoicingService.updateInvoiceDto(binder.getBean(), originalDto);
                if (!stay) {
                    UI.getCurrent().navigate(ContractSalesDetailView.ROUTE +
                            "/" + binder.getBean().getContractDto().getId());
                }
                SuccessNotification.showSavingSuccess(appEnv);
            } catch (SystemException e) {
                ErrorNotification.show(e, appEnv);
            }
        }
    }

    private void forwardForInvoicingAction() {
        if (binder.validate().isOk()) {
            String description = binder.getBean().getDescription();
            if (description == null || "".equals(description.trim())) {
                ErrorNotification.show(Transl.get("Description cannot be empty for this action"), appEnv);
                return;
            }
            InvoiceDto curDto = binder.getBean();
            curDto.setState(InvoiceState.TO_BE_INVOICED);
            binder.setBean(curDto);
            saveInvoice(true);
            checkStatusForButtons();
        }
    }

    private void refreshTitle(String invoiceNo) {
        title = new H2(Transl.get("Invoice"));

        if (invoiceNo != null && !"".equals(invoiceNo)) {
            title.setText(Transl.get("Invoice") + " - " + invoiceNo);
        }
    }

    private void checkStatusForButtons() {
        forwardForInvoicing.setEnabled(false);
        forwardForInvoicing.setVisible(false);
        invoiced.setEnabled(false);
        invoiced.setVisible(false);
        payed.setEnabled(false);
        payed.setVisible(false);
        invoiceComponent.setDescriptionRequired(true);
        InvoiceDto curDto = binder.getBean();
        if (InvoiceState.NEW == curDto.getState()) {
            invoiceComponent.setDescriptionRequired(false);
        }
        if (InvoiceState.NEW == curDto.getState() && curDto.getUserDto() != null &&
                curDto.getUserDto().getId().equals(SecurityUtils.getCurrentUserId())) {
            forwardForInvoicing.setEnabled(true);
            forwardForInvoicing.setVisible(true);
        }
        if (InvoiceState.TO_BE_INVOICED == curDto.getState() &&
                SecurityUtils.hasCustomPermission(DomainEnum.INVOICE_DOMAIN_NAME.getValue(),
                        String.valueOf(curDto.getContractDto().getId()),
                        Permission.INVOICE_TO_BE_INVOICED_ACTION.name(),
                        String.valueOf(curDto.getId()))) {
            invoiced.setEnabled(true);
            invoiced.setVisible(true);
        }
        if (InvoiceState.INVOICED == curDto.getState() &&
                SecurityUtils.hasCustomPermission(DomainEnum.INVOICE_DOMAIN_NAME.getValue(),
                        String.valueOf(curDto.getContractDto().getId()),
                        Permission.INVOICE_PAYED_ACTION.name(),
                        String.valueOf(curDto.getId()))) {
            payed.setEnabled(true);
            payed.setVisible(true);
        }
    }

    @Override
    public ConfirmAction getInvoicedAction(Binder<InvoiceDto> tempBinder, AppDialog dialog) {
        return () -> {
            if (tempBinder.validate().isOk()) {
                InvoiceDto tempDto = tempBinder.getBean();
                InvoiceDto curDto = binder.getBean();
                curDto.setState(InvoiceState.INVOICED);
                curDto.setIssueDate(tempDto.getIssueDate());
                curDto.setTaxDate(tempDto.getTaxDate());
                curDto.setInvoiceNo(tempDto.getInvoiceNo());
                binder.setBean(curDto);
                invoiceComponent.changeDueDate(curDto.getDaysToPay());
                refreshTitle(tempDto.getInvoiceNo());
                saveInvoice(true);
                checkStatusForButtons();
                if (dialog != null) {
                    dialog.close();
                }
            }
        };
    }

    @Override
    public ConfirmAction getPayedAction(Binder<InvoiceDto> tempBinder, AppDialog dialog) {
        return () -> {
            if (tempBinder.validate().isOk()) {
                InvoiceDto tempDto = tempBinder.getBean();
                InvoiceDto curDto = binder.getBean();
                curDto.setState(InvoiceState.PAYED);
                curDto.setPaymentDate(tempDto.getPaymentDate());
                binder.setBean(curDto);
                saveInvoice(true);
                checkStatusForButtons();
                if (dialog != null) {
                    dialog.close();
                }
            }
        };
    }
}
