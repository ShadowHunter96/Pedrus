package cz.bbn.cerberus.employee.ui.component;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.field.AppEmailField;
import cz.bbn.cerberus.commons.component.ui.field.AppPhoneField;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.validator.KeyValidator;
import cz.bbn.cerberus.commons.validator.MinMaxValidator;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.util.ArrayList;
import java.util.List;

public class EmployeeDetailComponent extends VerticalLayout {

    private final Binder<EmployeeDto> binder;
    private final EmployeeDto dto;
    private final boolean readOnly;
    private final List<UserDto> userList;

    public EmployeeDetailComponent(Binder<EmployeeDto> binder, boolean readOnly, List<UserDto> userList) {
        this.binder = binder;
        dto = binder.getBean();
        this.readOnly = readOnly;
        this.userList = userList;
        initComponent();
    }

    private void initComponent() {

        setMargin(false);
        setPadding(false);
        FormLayout formLayout = new FormLayout();

        TextField id = new TextField(Transl.get("Id"));
        id.setMaxLength(20);
        binder.forField(id).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY)).withValidator(new KeyValidator())
                .withValidator(new MinMaxValidator(2, 20)).bind(EmployeeDto::getId, EmployeeDto::setId);
        formLayout.add(id);

        TextField firstName = new TextField(Transl.get("First name"));
        firstName.setMaxLength(100);
        binder.forField(firstName).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(EmployeeDto::getFirstName, EmployeeDto::setFirstName);
        formLayout.add(firstName);

        TextField lastName = new TextField(Transl.get("Last name"));
        lastName.setMaxLength(100);
        binder.forField(lastName).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(EmployeeDto::getLastName, EmployeeDto::setLastName);
        formLayout.add(lastName);

        AppEmailField<EmployeeDto> companyEmail = new AppEmailField<>(Transl.get("Company email"));
        companyEmail.bind(binder, EmployeeDto::getCompanyEmail, EmployeeDto::setCompanyEmail, false);
        formLayout.add(companyEmail);

        AppEmailField<EmployeeDto> personalEmail = new AppEmailField<>(Transl.get("Personal email"));
        personalEmail.bind(binder, EmployeeDto::getPersonalEmail, EmployeeDto::setPersonalEmail, false);
        formLayout.add(personalEmail);

        AppPhoneField companyPhoneNumber = new AppPhoneField(Transl.get("Company phone number"));
        binder.forField(companyPhoneNumber)
                .bind(EmployeeDto::getCompanyPhoneNumber, EmployeeDto::setCompanyPhoneNumber);
        formLayout.add(companyPhoneNumber);

        AppPhoneField personalPhoneNumber = new AppPhoneField(Transl.get("Personal phone number"));
        binder.forField(personalPhoneNumber)
                .bind(EmployeeDto::getPersonalPhoneNumber, EmployeeDto::setPersonalPhoneNumber);
        formLayout.add(personalPhoneNumber);

        TextField accountNumber = new TextField(Transl.get("Account number"));
        accountNumber.setMaxLength(100);
        binder.forField(accountNumber).bind(EmployeeDto::getAccountNumber, EmployeeDto::setAccountNumber);
        formLayout.add(accountNumber);

        TextField position = new TextField(Transl.get("Position"));
        position.setMaxLength(100);
        binder.forField(position).bind(EmployeeDto::getPosition, EmployeeDto::setPosition);
        formLayout.add(position);

        DatePicker startDate = VaadinComponents.getDatePicker(Transl.get("Starting date"), dto.getStartDate());
        binder.forField(startDate).bind(EmployeeDto::getStartDate, EmployeeDto::setStartDate);
        formLayout.add(startDate);

        Checkbox active = new Checkbox(Transl.get("Active"));
        binder.forField(active).bind(EmployeeDto::getActive, EmployeeDto::setActive);
        active.setValue(Boolean.TRUE.equals(dto.getActive()));
        formLayout.add(active);

        DatePicker dismissDate = VaadinComponents.getDatePicker(Transl.get("Dismiss date"), dto.getDismissDate());
        binder.forField(dismissDate).bind(EmployeeDto::getDismissDate, EmployeeDto::setDismissDate);
        formLayout.add(dismissDate);

        UserDto userDto = new UserDto();
        userDto.setName(Transl.get(TextValues.EMPTY_VALUE));
        List<UserDto> tempUserList = new ArrayList<>();
        tempUserList.add(userDto);
        tempUserList.addAll(userList);

        ComboBox<UserDto> lineManager = new ComboBox<>(Transl.get("Line manager"));
        lineManager.setItemLabelGenerator(UserDto::getName);
        lineManager.setItems(tempUserList);
        binder.forField(lineManager).bind(EmployeeDto::getLineManagerUserDto, EmployeeDto::setLineManagerUserDto);
        formLayout.add(lineManager);

        ComboBox<UserDto> superior = new ComboBox<>(Transl.get("Superior"));
        superior.setItemLabelGenerator(UserDto::getName);
        superior.setItems(userList);
        binder.forField(superior).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(EmployeeDto::getSuperiorUserDto, EmployeeDto::setSuperiorUserDto);
        formLayout.add(superior);

        if (dto.getId() != null) {
            id.setReadOnly(true);
        }

        if (readOnly) {
            firstName.setReadOnly(true);
            lastName.setReadOnly(true);
            companyEmail.setReadOnly(true);
            personalEmail.setReadOnly(true);
            companyPhoneNumber.setReadOnly(true);
            personalPhoneNumber.setReadOnly(true);
            accountNumber.setReadOnly(true);
            position.setReadOnly(true);
            startDate.setReadOnly(true);
            active.setReadOnly(true);
            dismissDate.setReadOnly(true);
            lineManager.setReadOnly(true);
            superior.setReadOnly(true);
        }

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        this.add(formLayout);
    }
}
