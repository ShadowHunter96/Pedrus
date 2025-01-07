package cz.bbn.cerberus.employee.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.employee.dto.EmployeeActive;
import cz.bbn.cerberus.employee.dto.EmployeeByObjectFilterDto;
import cz.bbn.cerberus.translation.Transl;

public class EmployeeByObjectFilterComponent extends FormLayout {

    private final Button search;

    private ComboBox<EmployeeActive> employeeActive;

    public EmployeeByObjectFilterComponent(Button search) {
        this.search = search;
        initComponent();
    }

    private void initComponent(){
        employeeActive = new ComboBox<>(Transl.get("Active employee"));
        employeeActive.setItems(EmployeeActive.values());
        employeeActive.setItemLabelGenerator(employeeActive1 -> Transl.get(employeeActive1.name()));
        employeeActive.setValue(EmployeeActive.ACTIVE);
        this.add(employeeActive);

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public EmployeeByObjectFilterDto getEmployeeByObjectFilterDto(){
        EmployeeByObjectFilterDto filter = new EmployeeByObjectFilterDto();
        filter.setEmployeeActive(employeeActive.getValue());
        return filter;
    }

}
