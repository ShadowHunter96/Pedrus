package cz.bbn.cerberus.employee.ui;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.document.DocumentComponentOperation;
import cz.bbn.cerberus.employee.EmployeeComponentOperation;
import cz.bbn.cerberus.employee.EmployeeService;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employee.ui.component.EmployeeTabsComponent;
import cz.bbn.cerberus.employee.ui.component.tab.EmployeeDetailTab;
import cz.bbn.cerberus.employee.ui.component.tab.EmployeeDocumentTab;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

@Route(value = EmployeeDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.EMPLOYEE_VIEW)
@Slf4j
public class EmployeeDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "employee-detail";

    private final EmployeeService employeeService;
    private final DocumentComponentOperation documentComponentOperation;
    private final EmployeeComponentOperation employeeComponentOperation;
    private final AppEnv appEnv;
    private final ListService listService;
    private final EntityNewComponentOperation entityNewComponentOperation;

    private EmployeeDto dto;
    private boolean readOnly = true;

    public EmployeeDetailView(EmployeeService employeeService, DocumentComponentOperation documentComponentOperation,
                              EmployeeComponentOperation employeeComponentOperation, AppEnv appEnv,
                              ListService listService, EntityNewComponentOperation entityNewComponentOperation) {
        this.employeeService = employeeService;
        this.documentComponentOperation = documentComponentOperation;
        this.employeeComponentOperation = employeeComponentOperation;
        this.appEnv = appEnv;
        this.listService = listService;
        this.entityNewComponentOperation = entityNewComponentOperation;
    }

    private void initView() {
        removeAll();

        List<TabEntry> tabEntryList = new ArrayList<>();

        String heading = dto.getId() == null ? Transl.get("New employee") :
                Transl.get("Employee").concat(" - ").concat(dto.getFirstName()).concat(" ").concat(dto.getLastName());

        tabEntryList.add(new TabEntry(Transl.get("Employee detail"), new EmployeeDetailTab(
                dto, employeeComponentOperation.getEmployeeDtoSaveAction(null), readOnly, appEnv, listService.getUserDtoList())));

        Button addDocument = VaadinComponents.getNewButton(Transl.get("Add document"), false);
        if (SecurityUtils.hasPermission(Permission.DOCUMENT_VIEW) &&
                SecurityUtils.hasPermission(Permission.EMPLOYEE_DOCUMENT_VIEW)) {
            tabEntryList.add(new TabEntry(Transl.get("Documents"), new EmployeeDocumentTab(
                    documentComponentOperation, dto, !readOnly, appEnv, addDocument, listService)));
        }

        EmployeeTabsComponent tabsComponent =
                new EmployeeTabsComponent(heading, tabEntryList, !readOnly, addDocument, entityNewComponentOperation);
        tabsComponent.addNewEntitySlideTab(new NewEntityButtonsComponent(
                entityNewComponentOperation, EntityNewType.BACKOFFICE));
        add(tabsComponent);

    }

    private void setParameter(String parameter) throws SystemException {
        if (SecurityUtils.hasPermission(Permission.EMPLOYEE_VIEW)) {
            dto = employeeService.getEmployee(parameter);
            refreshBreadcrumbText(AppUtils.cutText(dto.getFirstName(), 2, false)
                    .concat(AppUtils.cutText(dto.getLastName(), 2, false)));
            if (SecurityUtils.hasPermission(Permission.EMPLOYEE_EDIT)) {
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

    @Override
    public void setParameter(BeforeEvent event, String parameter) {
        if (parameter != null) {
            try {
                setParameter(parameter);
            } catch (SystemException ex) {
                log.error(TextValues.SYSTEM_EXCEPTION, ex);
                ErrorNotification.show(ex, appEnv);
            }
        }
    }
}
