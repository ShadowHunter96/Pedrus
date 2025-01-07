package cz.bbn.cerberus.invoice.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologyComponentOperation;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.SuccessNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.ContractComponentOperation;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.invoice.InvoiceComponentOperation;
import cz.bbn.cerberus.invoice.dto.InvoicingDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;
import java.util.List;

public class AddInvoicingDialog extends AppDialog {

    private final Binder<InvoicingDto> binderOneTime = new Binder<>();
    private final Binder<InvoicingDto> binderPeriodic = new Binder<>();
    private final SubjectDto subjectDto;
    private final InvoiceComponentOperation invoiceComponentOperation;
    private final SubjectComponentOperation subjectComponentOperation;
    private final ContractComponentOperation contractComponentOperation;
    private final AppInfiniteGrid<?> grid;
    private final AppEnv appEnv;
    private final AreaTechnologyComponentOperation areaTechnologyComponentOperation;

    private ContractDto contractDto;

    public AddInvoicingDialog(ContractDto contractDto, SubjectDto subjectDto,
                              InvoiceComponentOperation invoiceComponentOperation,
                              SubjectComponentOperation subjectComponentOperation,
                              ContractComponentOperation contractComponentOperation,
                              AppInfiniteGrid<?> grid, AppEnv appEnv,
                              AreaTechnologyComponentOperation areaTechnologyComponentOperation) {
        this.contractDto = contractDto;
        this.subjectDto = subjectDto;
        this.invoiceComponentOperation = invoiceComponentOperation;
        this.subjectComponentOperation = subjectComponentOperation;
        this.contractComponentOperation = contractComponentOperation;
        this.grid = grid;
        this.appEnv = appEnv;
        this.areaTechnologyComponentOperation = areaTechnologyComponentOperation;
        initTab();
    }

    private void initTab() {
        setTitle(Transl.get("Invoicing"));

        List<SubjectDto> subjectDtoList = subjectComponentOperation.getMySubjects();
        List<ContractDto> contractList = contractComponentOperation.getMyInvoiceEditContractList();

        List<TabEntry> tabEntryList = new ArrayList<>();
        if (contractDto != null && !SecurityUtils.hasCustomPermission(
                DomainEnum.INVOICE_DOMAIN_NAME.getValue(), contractDto.getId(), Permission.INVOICE_EDIT.name())) {
            contractDto = null;
        }

        tabEntryList.add(new TabEntry(Transl.get("One time invoice"),
                new InvoicingTab(subjectDto, contractDto, contractList, subjectDtoList, binderOneTime,
                        invoiceComponentOperation, false, areaTechnologyComponentOperation)));

        tabEntryList.add(new TabEntry(Transl.get("Periodic invoice"),
                new InvoicingTab(subjectDto, contractDto, contractList, subjectDtoList, binderPeriodic,
                        invoiceComponentOperation, true, areaTechnologyComponentOperation)));

        InvoicingTabsComponent invoicingTabsComponent = new InvoicingTabsComponent(tabEntryList);
        this.setContent(invoicingTabsComponent);

        getContent().setMargin(false);
        getContent().setPadding(false);
        setWidth("852px");
        setHeight("950px");

        Button saveButton = VaadinComponents.getSubmitButton();
        saveButton.setDisableOnClick(true);
        saveButton.addClickListener(e -> {
            Binder<InvoicingDto> binder = invoicingTabsComponent.getSelectedTab() == 0 ? binderOneTime : binderPeriodic;
            if (binder.validate().isOk()) {
                InvoicingDto invoicingDto = binder.getBean();
                SaveAction<InvoicingDto> saveAction = invoiceComponentOperation.getSaveAction();
                saveAction.saveItem(invoicingDto, new InvoicingDto());
                this.close();
                if (grid != null) {
                    grid.loadData();
                }
                SuccessNotification.showSavingSuccess(appEnv);
            }
            saveButton.setEnabled(true);
        });

        Button closeButton = VaadinComponents.getCloseButton();
        closeButton.addClickListener(e -> this.close());

        addButtons(closeButton, saveButton);
    }
}
