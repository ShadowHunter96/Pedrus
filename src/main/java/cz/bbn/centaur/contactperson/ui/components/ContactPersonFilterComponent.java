package cz.bbn.cerberus.contactperson.ui.components;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contactperson.dto.ContactPersonFilterDto;
import cz.bbn.cerberus.contactperson.ui.ContactPersonView;
import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class ContactPersonFilterComponent extends FormLayout {

    private TextField id;
    private TextField firstName;
    private TextField lastName;
    private TextField email;
    private TextField phone;
    private ComboBox<ContactPersonTypeDto> contactPersonType;
    private ComboBox<ProjectDto> projectDtoComboBox;
    private ComboBox<SubjectDto> subjectDtoComboBox;
    private Checkbox showDeleted;

    private final List<ContactPersonTypeDto> contactPersonTypeDtoList;
    private final List<ProjectDto> projectDtoList;
    private final List<SubjectDto> subjectDtoList;
    private final Button search;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;

    public ContactPersonFilterComponent(
            List<ContactPersonTypeDto> contactPersonTypeDtoList, List<ProjectDto> projectDtoList,
            List<SubjectDto> subjectDtoList, Button search, String params, HistoryBreadcrumbs historyBreadcrumbs) {
        this.contactPersonTypeDtoList = contactPersonTypeDtoList;
        this.projectDtoList = projectDtoList;
        this.subjectDtoList = subjectDtoList;
        this.search = search;
        this.params = params;
        this.historyBreadcrumbs = historyBreadcrumbs;
        initComponents();
        fillFilterFromUrl();
    }

    public ContactPersonFilterComponent(List<ContactPersonTypeDto> contactPersonTypeDtoList, Button search) {
        this.contactPersonTypeDtoList = contactPersonTypeDtoList;
        this.search = search;
        this.projectDtoList = null;
        this.subjectDtoList = null;
        this.params = null;
        this.historyBreadcrumbs = null;
        initComponents();
    }

    private void initComponents() {
        id = new TextField(Transl.get("Id"));
        this.add(id);

        firstName = new TextField(Transl.get("First name"));
        this.add(firstName);

        lastName = new TextField(Transl.get("Last name"));
        this.add(lastName);

        email = new TextField(Transl.get("Email"));
        this.add(email);

        phone = new TextField(Transl.get("Phone"));
        this.add(phone);

        contactPersonType = new ComboBox<>(Transl.get("Contact type"));
        ContactPersonTypeDto contactPersonTypeDto = new ContactPersonTypeDto();
        contactPersonTypeDto.setName(Transl.get(TextValues.SHOW_ALL_TEXT_VALUE));
        contactPersonTypeDtoList.add(0, contactPersonTypeDto);
        contactPersonType.setItems(contactPersonTypeDtoList);
        contactPersonType.setValue(contactPersonTypeDto);
        contactPersonType.setItemLabelGenerator(ContactPersonTypeDto::getName);
        this.add(contactPersonType);

        if (projectDtoList != null && subjectDtoList != null) {
            projectDtoComboBox = new ComboBox<>(Transl.get("Project"));
            ProjectDto projectDto = new ProjectDto();
            projectDto.setName(Transl.get(TextValues.SHOW_ALL_TEXT_VALUE));
            List<ProjectDto> actualProjectDtoList = new ArrayList<>(Collections.singleton(projectDto));
            actualProjectDtoList.addAll(projectDtoList);
            projectDtoComboBox.setItems(actualProjectDtoList);
            projectDtoComboBox.setValue(projectDto);
            projectDtoComboBox.setItemLabelGenerator(ProjectDto::getName);
            this.add(projectDtoComboBox);
            subjectDtoComboBox = new ComboBox<>(Transl.get("Subject"));
            SubjectDto subjectDto = new SubjectDto();
            subjectDto.setName(Transl.get(TextValues.SHOW_ALL_TEXT_VALUE));
            List<SubjectDto> actualSubjectDtoList = new ArrayList<>(Collections.singleton(subjectDto));
            actualSubjectDtoList.addAll(subjectDtoList);
            subjectDtoComboBox.setItems(actualSubjectDtoList);
            subjectDtoComboBox.setValue(subjectDto);
            subjectDtoComboBox.setItemLabelGenerator(SubjectDto::getName);
            this.add(subjectDtoComboBox);

            showDeleted = new Checkbox(Transl.get("Show deleted"));
            if (SecurityUtils.hasPermission(Permission.CONTACT_PERSON_SHOW_DELETED)) {
                this.add(showDeleted);
            }
        }

        this.add(search);
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    private void fillFilterFromUrl() {
        Map<String, String> map = AppUtils.getMapByParams(params);
        if (map.containsKey("id")) {
            id.setValue(map.get("id"));
        }
        if (map.containsKey("firstName")) {
            firstName.setValue(map.get("firstName"));
        }
        if (map.containsKey("lastName")) {
            lastName.setValue(map.get("lastName"));
        }
        if (map.containsKey("email")) {
            email.setValue(map.get("email"));
        }
        if (map.containsKey("phone")) {
            phone.setValue(map.get("phone"));
        }
        if (map.containsKey("contactPersonType")) {
            for (ContactPersonTypeDto contactPersonTypeDto : contactPersonTypeDtoList) {
                if (contactPersonTypeDto.getId() != null
                        && contactPersonTypeDto.getId().equals(map.get("contactPersonType"))) {
                    contactPersonType.setValue(contactPersonTypeDto);
                }
            }
        }
        if (map.containsKey("projectDto")) {
            for (ProjectDto projectDto : projectDtoList) {
                if (projectDto.getId() != null && projectDto.getId().equals(map.get("projectDto"))) {
                    projectDtoComboBox.setValue(projectDto);
                }
            }
        }
        if (map.containsKey("subjectDto")) {
            for (SubjectDto subjectDto : subjectDtoList) {
                if (subjectDto.getId() != null && subjectDto.getId().equals(map.get("subjectDto"))) {
                    subjectDtoComboBox.setValue(subjectDto);
                }
            }
        }
        if (map.containsKey("showDeleted") && SecurityUtils.hasPermission(Permission.CONTACT_PERSON_SHOW_DELETED)) {
            showDeleted.setValue("true".equalsIgnoreCase(map.get("showDeleted")));
        }
    }

    public void fillUrl() {
        String paramUrl = ContactPersonView.ROUTE.concat("/");
        if (id.getValue() != null && !id.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&id=").concat(id.getValue());
        }
        if (firstName.getValue() != null && !firstName.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&firstName=").concat(firstName.getValue());
        }
        if (lastName.getValue() != null && !lastName.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&lastName=").concat(lastName.getValue());
        }
        if (email.getValue() != null && !email.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&email=").concat(email.getValue());
        }
        if (phone.getValue() != null && !phone.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&phone=").concat(phone.getValue());
        }
        if (contactPersonType.getValue() != null && contactPersonType.getValue().getId() != null) {
            paramUrl = paramUrl.concat("&contactPersonType=").concat(contactPersonType.getValue().getId());
        }
        if (projectDtoComboBox.getValue() != null && projectDtoComboBox.getValue().getId() != null) {
            paramUrl = paramUrl.concat("&projectDto=").concat(projectDtoComboBox.getValue().getId());
        }
        if (subjectDtoComboBox.getValue() != null && subjectDtoComboBox.getValue().getId() != null) {
            paramUrl = paramUrl.concat("&subjectDto=").concat(subjectDtoComboBox.getValue().getId());
        }
        if (showDeleted.getValue() != null && SecurityUtils.hasPermission(Permission.CONTACT_PERSON_SHOW_DELETED)) {
            paramUrl = paramUrl.concat("&showDeleted=").concat(showDeleted.getValue().toString());
        }

        UI.getCurrent().getPage().getHistory().pushState(null, paramUrl);
        historyBreadcrumbs.refreshLastBreadcrumb(paramUrl);
    }

    public ContactPersonFilterDto getContactPersonFilterDto() {
        ContactPersonFilterDto dto = new ContactPersonFilterDto();
        dto.setId(id.getValue());
        dto.setFirstName(firstName.getValue());
        dto.setLastName(lastName.getValue());
        dto.setEmail(email.getValue());
        dto.setPhone(phone.getValue());
        dto.setContactPersonType(contactPersonType.getValue().getId());
        if (projectDtoComboBox != null && subjectDtoList != null) {
            dto.setProjectId(projectDtoComboBox.getValue().getId());
            dto.setSubjectId(subjectDtoComboBox.getValue().getId());
            dto.setShowDeleted(showDeleted.getValue());
        }
        return dto;
    }
}
