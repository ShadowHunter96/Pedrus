package cz.bbn.cerberus.employee.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.MultiSelectComboBox;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employee.dto.EmployeeLinkDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class EmployeeLinkDialog extends AppDialog {

    private final SaveAction<EmployeeLinkDto> saveAction;
    private final EmployeeByObjectGridComponent grid;
    private final ListService listService;
    private final EmployeeLinkDto dto;
    private final AppEnv appEnv;
    private final List<String> linkedEmployeeIdList;

    public EmployeeLinkDialog(SaveAction<EmployeeLinkDto> saveAction,
                              EmployeeByObjectGridComponent grid, ListService listService,
                              EmployeeLinkDto dto, AppEnv appEnv, List<String> linkedEmployeeIdList) {
        this.dto = dto;
        this.saveAction = saveAction;
        this.grid = grid;
        this.listService = listService;
        this.appEnv = appEnv;
        this.linkedEmployeeIdList = linkedEmployeeIdList;
        initComponent();
    }

    private void initComponent() {
        Binder<EmployeeLinkDto> binder = new Binder<>();
        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.setMinHeight("7em");
        setTitle(Transl.get("Link team member"));
        List<EmployeeDto> employeeDtoList = listService.getEmployeeDtoList();
        employeeDtoList.removeIf(employeeDto -> linkedEmployeeIdList.contains(employeeDto.getId()));
        MultiSelectComboBox<EmployeeDto> employee = new MultiSelectComboBox<>(Transl.get("Team member"));
        employee.setItems(employeeDtoList);
        employee.setItemLabelGenerator(employeeDto ->
                employeeDto.getLastName()
                        .concat(" ")
                .concat(employeeDto.getFirstName()));
        binder.forField(employee).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(EmployeeLinkDto::getEmployeeDtoSet, EmployeeLinkDto::setEmployeeDtoSet);
        employee.setWidth("20em");
        verticalLayout.add(employee);
        setContent(verticalLayout);

        binder.setBean(dto);
        Button submit = VaadinComponents.getSubmitButton();
        submit.addClickListener(buttonClickEvent -> {
            if(binder.validate().isOk()) {
                dto.setEmployeeDtoSet(employee.getSelectedItems());
                saveAction.saveItem(dto, null);
                grid.loadData();
                this.close();
            } else {
                ErrorNotification.show(Transl.get(TextValues.INVALIS_INPUT), appEnv);
            }
        });

        addCloseButton();
        addSubmitButton(submit);
    }
}
