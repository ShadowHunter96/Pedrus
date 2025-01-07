package cz.bbn.cerberus.employeecontract.ui;

import com.vaadin.flow.router.BeforeEvent;
import com.vaadin.flow.router.HasUrlParameter;
import com.vaadin.flow.router.Route;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.employeecontract.EmployeeContractComponentOperation;
import cz.bbn.cerberus.employeecontract.dto.EmployeeContractDto;
import cz.bbn.cerberus.employeecontract.ui.component.EmployeeContractTabsComponent;
import cz.bbn.cerberus.employeecontract.ui.component.tab.EmployeeContractDetailTab;
import cz.bbn.cerberus.employeecontract.ui.component.tab.EmployeeContractLinkedContractTab;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.SerializationUtils;

import java.util.ArrayList;
import java.util.List;

@Route(value = EmployeeContractDetailView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.EMPLOYEE_CONTRACT_VIEW)
@Slf4j
public class EmployeeContractDetailView extends AppView implements HasUrlParameter<String> {

    public static final String ROUTE = "employee-contact-detail";

    private final EmployeeContractComponentOperation componentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;
    private final AppEnv appEnv;

    public EmployeeContractDetailView(EmployeeContractComponentOperation componentOperation,
                                      EntityNewComponentOperation entityNewComponentOperation, AppEnv appEnv) {
        this.componentOperation = componentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        this.appEnv = appEnv;
    }

    private void initView(EmployeeContractDto dto) {
        removeAll();

        EmployeeContractDto originalDto = SerializationUtils.clone(dto);

        String heading;
        if (dto.getId() != null) {
            heading = Transl.get("Employee contract - {0}", dto.getId());
        } else {
            heading = Transl.get("New employee contract");
        }

        List<TabEntry> tabEntryList = new ArrayList<>();
        tabEntryList.add(new TabEntry(Transl.get("Employee contract detail"), new EmployeeContractDetailTab(
                dto, componentOperation.getSaveAction(null), componentOperation.getOwnCompanyList(),
                componentOperation.getTypeList(), componentOperation.getEmployeeList(),
                componentOperation.getEmpConListExceptId(dto.getId()), componentOperation,
                componentOperation.getStateList(), appEnv)));

        tabEntryList.add(new TabEntry(Transl.get("Linked employee contracts"),
                new EmployeeContractLinkedContractTab(componentOperation.getLinkedEmpConItemActions(dto), appEnv)));

        EmployeeContractTabsComponent tabsComponent = new EmployeeContractTabsComponent(
                heading, tabEntryList, entityNewComponentOperation, componentOperation, originalDto);

        add(tabsComponent);
    }

    @Override
    public void setParameter(BeforeEvent event, String param) {
        try {
            EmployeeContractDto dto = componentOperation.getEmployeeContract(param);
            refreshBreadcrumbText(dto.getId());
            initView(dto);
        } catch (SystemException ex) {
            log.error(TextValues.SYSTEM_EXCEPTION, ex);
            ErrorNotification.show(ex, appEnv);
        }

    }
}
