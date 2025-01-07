package cz.bbn.cerberus.employee.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.employee.dto.EmployeeFilterDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;

public class EmployeeFilterComponent extends FormLayout {

    private final Binder<EmployeeFilterDto> binder;

    public EmployeeFilterComponent(Button search) {
        binder = new Binder<>();

        TextField id = new TextField(Transl.get("Id"));
        binder.forField(id).bind(EmployeeFilterDto::getId, EmployeeFilterDto::setId);
        this.add(id);

        TextField firstName = new TextField(Transl.get("First name"));
        binder.forField(firstName).bind(EmployeeFilterDto::getFirstName, EmployeeFilterDto::setFirstName);
        this.add(firstName);

        TextField lastName = new TextField(Transl.get("Last name"));
        binder.forField(lastName).bind(EmployeeFilterDto::getLastName, EmployeeFilterDto::setLastName);
        this.add(lastName);

        TextField companyEmail = new TextField(Transl.get("Company email"));
        binder.forField(companyEmail).bind(EmployeeFilterDto::getCompanyEmail, EmployeeFilterDto::setCompanyEmail);
        this.add(companyEmail);

        TextField companyPhoneNumber = new TextField(Transl.get("Company phone number"));
        binder.forField(companyPhoneNumber)
                .bind(EmployeeFilterDto::getCompanyPhoneNumber, EmployeeFilterDto::setCompanyPhoneNumber);
        this.add(companyPhoneNumber);

        Checkbox showDeleted = new Checkbox(Transl.get("Show deleted"));
        if (SecurityUtils.hasPermission(Permission.EMPLOYEE_DELETE)) {
            binder.forField(showDeleted).bind(EmployeeFilterDto::isShowDeleted, EmployeeFilterDto::setShowDeleted);
            this.add(showDeleted);
        }

        binder.setBean(new EmployeeFilterDto());

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public EmployeeFilterDto getEmployeeFilterDto() {
        return binder.getBean();
    }
}
