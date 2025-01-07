package cz.bbn.cerberus.employee.ui.component.tab;

import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.tab.TabDtoComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employee.ui.component.EmployeeDetailComponent;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.List;

public class EmployeeDetailTab extends TabDtoComponent<EmployeeDto> {

    private final boolean readOnly;
    private final List<UserDto> userList;

    public EmployeeDetailTab(EmployeeDto dto, SaveAction<EmployeeDto> saveAction,
                             boolean readOnly, AppEnv appEnv, List<UserDto> userList) {
        super(dto, saveAction, appEnv);
        this.readOnly = readOnly;
        this.userList = userList;
        initTab();
    }

    @Override
    protected void initTab() {
        removeAll();

        this.setId(RobotFrameworkVariables.EMPLOYEE_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        EmployeeDetailComponent employeeDetailComponent = new EmployeeDetailComponent(super.getBinder(), readOnly, userList);

        HorizontalLayout mainHorizontalLayout = new HorizontalLayout();
        mainHorizontalLayout.setSizeFull();
        mainHorizontalLayout.add(employeeDetailComponent);

        this.add(mainHorizontalLayout);
    }
}
