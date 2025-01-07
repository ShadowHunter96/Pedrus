package cz.bbn.cerberus.contactperson.ui.components.tab;

import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.CssVariables;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.field.AppEmailField;
import cz.bbn.cerberus.commons.component.ui.field.AppPhoneField;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.component.ui.tab.TabDtoComponent;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.validator.PhoneValidator;
import cz.bbn.cerberus.contactperson.dto.ContactPersonDto;
import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.phoneprefix.dto.PhonePrefixDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class ContactPersonDetailTab extends TabDtoComponent<ContactPersonDto> {

    private final List<ContactPersonTypeDto> contactPersonTypeDtoList;
    private final List<PhonePrefixDto> phonePrefixDtoList;
    private final boolean isDialog;
    private final boolean readOnly;

    public ContactPersonDetailTab(
            ContactPersonDto dto, SaveAction<ContactPersonDto> saveAction,
            List<ContactPersonTypeDto> contactPersonTypeDtoList,
            AppEnv appEnv, List<PhonePrefixDto> phonePrefixDtoList, boolean isDialog, boolean readOnly) {
        super(dto, saveAction, appEnv);
        this.contactPersonTypeDtoList = contactPersonTypeDtoList;
        this.phonePrefixDtoList = phonePrefixDtoList;
        this.isDialog = isDialog;
        this.readOnly = readOnly;
        initTab();
    }

    @Override
    protected void initTab() {
        removeAll();
        setMargin(false);
        setPadding(false);

        this.setId(RobotFrameworkVariables.CONTACT_PERSON_DETAIL_CARD_ID.getValue());
        this.setSizeFull();

        FormLayout formLayout = new FormLayout();

        TextField firstName = new TextField(Transl.get("First name"));
        firstName.setMaxLength(100);
        getBinder().forField(firstName).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContactPersonDto::getFirstName, ContactPersonDto::setFirstName);
        formLayout.add(firstName);

        TextField lastName = new TextField(Transl.get("Last name"));
        lastName.setMaxLength(100);
        getBinder().forField(lastName).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContactPersonDto::getLastName, ContactPersonDto::setLastName);
        formLayout.add(lastName);

        AppEmailField<ContactPersonDto> email = new AppEmailField<>(Transl.get("Email"));
        email.bind(getBinder(), ContactPersonDto::getEmail, ContactPersonDto::setEmail);
        email.setRequired(true);
        formLayout.add(email);

        ComboBox<ContactPersonTypeDto> contactPersonType = new ComboBox<>(Transl.get("Position"));
        contactPersonType.setItems(contactPersonTypeDtoList);
        contactPersonType.setItemLabelGenerator(ContactPersonTypeDto::getName);
        contactPersonType.setValue(getDto().getContactPersonType());

        getBinder().forField(contactPersonType).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContactPersonDto::getContactPersonType, ContactPersonDto::setContactPersonType);
        formLayout.add(contactPersonType);

        TextField position = new TextField(Transl.get("Position name"));
        position.setMaxLength(100);
        getBinder().forField(position).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContactPersonDto::getContactPersonPosition, ContactPersonDto::setContactPersonPosition);
        formLayout.add(position);

        ComboBox<PhonePrefixDto> phonePrefix1 = new ComboBox<>(Transl.get("Phone prefix 1"));
        phonePrefix1.setItems(phonePrefixDtoList);
        phonePrefix1.setItemLabelGenerator(contactPersonTypeDto ->
                contactPersonTypeDto.getCountryCode().concat("  ").concat(contactPersonTypeDto.getPhonePrefix()));
        getBinder().forField(phonePrefix1).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContactPersonDto::getPhonePrefixDto, ContactPersonDto::setPhonePrefixDto);

        AppPhoneField phone1 = new AppPhoneField(Transl.get("Phone 1"));
        getBinder().forField(phone1)
                .withValidator(new PhoneValidator()).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContactPersonDto::getPhone, ContactPersonDto::setPhone);
        formLayout.add(phonePrefix1, phone1);


        ComboBox<PhonePrefixDto> phonePrefix2 = new ComboBox<>(Transl.get("Phone prefix 2"));
        phonePrefix2.setItems(phonePrefixDtoList);
        phonePrefix2.setItemLabelGenerator(contactPersonTypeDto ->
                contactPersonTypeDto.getCountryCode().concat("  ").concat(contactPersonTypeDto.getPhonePrefix()));
        getBinder().forField(phonePrefix2).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(ContactPersonDto::getPhonePrefix2Dto, ContactPersonDto::setPhonePrefix2Dto);

        AppPhoneField phone2 = new AppPhoneField(Transl.get("Phone 2"));
        getBinder().forField(phone2)
                .withValidator(new PhoneValidator())
                .bind(ContactPersonDto::getPhone2, ContactPersonDto::setPhone2);
        formLayout.add(phonePrefix2, phone2);

        TextArea otherContacts = new TextArea(Transl.get("Other contacts"));
        otherContacts.setMaxLength(255);
        getBinder().forField(otherContacts)
                .bind(ContactPersonDto::getOtherContacts, ContactPersonDto::setOtherContacts);
        otherContacts.setWidthFull();
        otherContacts.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());

        TextArea description = new TextArea(Transl.get("Description"));
        description.setMaxLength(VaadinComponents.DESCRIPTION_MAX_LENGTH);
        getBinder().forField(description)
                .bind(ContactPersonDto::getDescription, ContactPersonDto::setDescription);
        description.setWidthFull();
        description.setHeight(CssVariables.DEFAULT_TEXT_AREA_HEIGHT.getValue());

        VerticalLayout verticalLayout = new VerticalLayout();
        verticalLayout.add(formLayout, otherContacts, description);
        verticalLayout.setHeightFull();

        getBinder().setBean(getDto());

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);

        if (isDialog) {
            verticalLayout.setMargin(false);
            verticalLayout.setPadding(false);
        }

        if (readOnly) {
            firstName.setReadOnly(true);
            lastName.setReadOnly(true);
            email.setReadOnly(true);
            contactPersonType.setReadOnly(true);
            position.setReadOnly(true);
            phonePrefix1.setReadOnly(true);
            phone1.setReadOnly(true);
            otherContacts.setReadOnly(true);
            description.setReadOnly(true);
            phonePrefix2.setReadOnly(true);
            phone2.setReadOnly(true);
        }

        this.add(verticalLayout);
    }
}
