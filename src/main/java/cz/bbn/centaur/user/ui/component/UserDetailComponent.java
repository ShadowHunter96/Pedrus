package cz.bbn.cerberus.user.ui.component;

import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.appcard.AppDetailCardComponent;
import cz.bbn.cerberus.commons.component.ui.field.AppEmailField;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.employee.EmployeeComponentOperation;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.ui.UserView;

import java.util.List;

public class UserDetailComponent extends AppDetailCardComponent<UserDto> {

    private final EmployeeComponentOperation employeeComponentOperation;
    private final UserDto userDto;

    public UserDetailComponent(UserDto dto, SaveAction<UserDto> saveAction, boolean showSubmitButton,
                               EmployeeComponentOperation employeeComponentOperation, AppEnv appEnv,
                               EntityNewComponentOperation entityNewComponentOperation) {
        super(dto, saveAction, showSubmitButton, appEnv, entityNewComponentOperation);
        this.employeeComponentOperation = employeeComponentOperation;
        this.userDto = dto;
        initComponent();
    }

    @Override
    protected void initComponent() {
        String heading = Transl.get("User")
                .concat(" - ")
                .concat(getDto().getName());
        setHeading(heading);
        this.addBackButton(UserView.ROUTE);
        if (isShowSubmitButton()) {
            this.addSaveButton();
        }
        this.setId(RobotFrameworkVariables.USER_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        VerticalLayout verticalLayout = new VerticalLayout();
        FormLayout formLayout = new FormLayout();

        TextField idValue = new TextField(Transl.get("Id"));
        getBinder().forField(idValue).bind(UserDto::getStringId, null);
        idValue.setReadOnly(true);
        formLayout.add(idValue);

        TextField login = new TextField(Transl.get("Login"));
        getBinder().forField(login).bind(UserDto::getLogin, UserDto::setLogin);
        login.setReadOnly(true);
        formLayout.add(login);

        TextField azureId = new TextField(Transl.get("Azure id"));
        azureId.setMaxLength(50);
        getBinder().forField(azureId).bind(UserDto::getAzureId, UserDto::setAzureId);
        azureId.setReadOnly(true);
        formLayout.add(azureId);

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        getBinder().forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(UserDto::getName, UserDto::setName);
        formLayout.add(name);

        AppEmailField<UserDto> email = new AppEmailField<>(Transl.get("Email"));
        email.bind(getBinder(), UserDto::getMail, UserDto::setMail, false);
        formLayout.add(email);

        TextField preferredLanguage = new TextField(Transl.get("Preferred language"));
        preferredLanguage.setReadOnly(true);
        getBinder().forField(preferredLanguage).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(UserDto::getPreferredLanguage, UserDto::setPreferredLanguage);
        formLayout.add(preferredLanguage);

        List<EmployeeDto> employeeList = employeeComponentOperation.getEmployeeList(userDto.getEmployee());
        EmployeeDto employeeDto = new EmployeeDto();
        employeeDto.setFirstName(TextValues.EMPTY_VALUE);
        employeeList.add(0, employeeDto);

        UserDto dto = getDto();
        if (dto.getEmployee() == null) {
            dto.setEmployee(employeeDto);
        }

        ComboBox<EmployeeDto> employee = new ComboBox<>(Transl.get("Employee"));
        employee.setItems(employeeList);
        employee.setItemLabelGenerator(this::getEmployeeName);
        getBinder().forField(employee).bind(UserDto::getEmployee, UserDto::setEmployee);
        formLayout.add(employee);

        TextField acronym = new TextField(Transl.get("Acronym"));
        acronym.setMaxLength(4);
        getBinder().forField(acronym).bind(UserDto::getAcronym, UserDto::setAcronym);
        formLayout.add(acronym);

        Checkbox sendUnreadMails = new Checkbox(Transl.get("Send unread mails"));
        getBinder().forField(sendUnreadMails).bind(UserDto::getSendUnreadMails, UserDto::setSendUnreadMails);
        formLayout.add(sendUnreadMails);

        verticalLayout.add(formLayout);

        getBinder().setBean(dto);

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        this.add(verticalLayout);
    }

    private String getEmployeeName(EmployeeDto employeeDto) {
        return employeeComponentOperation.getEmployeeName(employeeDto);
    }

}
