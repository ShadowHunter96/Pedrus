package cz.bbn.cerberus.note.ui.component;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.datepicker.DatePicker;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.note.NoteComponentOperation;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.note.dto.NoteFilterDto;
import cz.bbn.cerberus.note.dto.NoteObjectIdNameDto;
import cz.bbn.cerberus.note.ui.NoteView;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class NoteFilterComponent extends FormLayout {

    private final NoteComponentOperation noteComponentOperation;

    private final Button searchButton;
    private final List<UserDto> userList;
    private final boolean extended;
    private final Set<String> permissionSet;
    private final String params;
    private final HistoryBreadcrumbs historyBreadcrumbs;
    private final NoteTypeEnum noteType;

    private TextField text;
    private DatePicker createdFrom;
    private DatePicker createdTo;
    private ComboBox<UserDto> createdBy;
    private ComboBox<NoteTypeEnum> objectNameComboBox = new ComboBox<>();
    private ComboBox<NoteObjectIdNameDto> objectIdComboBox = new ComboBox<>();
    private Checkbox showArchived = new Checkbox();
    private List<NoteTypeEnum> noteTypeEnumList;

    public NoteFilterComponent(Button searchButton, List<UserDto> userList, Set<String> permissionSet,
                               NoteTypeEnum noteType) {
        this.searchButton = searchButton;
        this.userList = userList;
        this.noteComponentOperation = null;
        this.extended = false;
        this.noteType = noteType;
        this.permissionSet = permissionSet;
        this.params = null;
        this.historyBreadcrumbs = null;
        initComponent();
    }

    public NoteFilterComponent(Button searchButton, List<UserDto> userList,
                               NoteComponentOperation noteComponentOperation, Set<String> permissionSet,
                               String params, HistoryBreadcrumbs historyBreadcrumbs) {
        this.searchButton = searchButton;
        this.userList = userList;
        this.noteComponentOperation = noteComponentOperation;
        this.extended = true;
        this.noteType = null;
        this.permissionSet = permissionSet;
        this.params = params;
        this.historyBreadcrumbs = historyBreadcrumbs;
        initComponent();
        fillFilterFromUrl();
    }

    private void initComponent() {

        setWidthFull();

        text = new TextField(Transl.get("Search"));

        createdFrom = VaadinComponents.getDatePicker(null);
        createdFrom.setLabel(Transl.get("From"));

        createdTo = VaadinComponents.getDatePicker(LocalDate.now());
        createdTo.setLabel(Transl.get("To"));

        createdFrom.setMax(createdTo.getValue());

        createdFrom.addValueChangeListener(e -> createdTo.setMin(e.getValue()));
        createdTo.addValueChangeListener(e -> createdFrom.setMax(e.getValue()));

        createdBy = new ComboBox<>(Transl.get("Created by"));
        UserDto emptyUser = new UserDto();
        emptyUser.setName(Transl.get(TextValues.SHOW_ALL_TEXT_VALUE));
        userList.add(0, emptyUser);
        createdBy.setItems(userList);
        createdBy.setItemLabelGenerator(UserDto::getName);
        createdBy.setValue(userList.get(0));

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);

        this.add(text, createdFrom, createdTo, createdBy);

        if (extended) {

            objectNameComboBox = new ComboBox<>(Transl.get("Object"));
            noteTypeEnumList = noteComponentOperation.getNoteTypeEnumList();
            noteTypeEnumList.add(0, NoteTypeEnum.ANY);
            objectNameComboBox.setItems(noteTypeEnumList);
            objectNameComboBox.setItemLabelGenerator(this::translateNoteTypeEnum);
            objectNameComboBox.setValue(noteTypeEnumList.get(0));

            objectIdComboBox = new ComboBox<>(Transl.get("Object ID"));
            initiateEmptyIdComboBox();
            objectIdComboBox.setReadOnly(true);

            objectNameComboBox.addValueChangeListener(e -> {
                if (TextValues.SHOW_ALL_TEXT_VALUE.equals(e.getValue().getObjectName())) {
                    initiateEmptyIdComboBox();
                    objectIdComboBox.setReadOnly(true);
                } else {
                    changeIdComboBox(e.getValue().getObjectName());
                    objectIdComboBox.setReadOnly(false);
                }
            });

            showArchived = new Checkbox(Transl.get("Show also archived"));
            this.add(objectNameComboBox, objectIdComboBox, showArchived);
        }

        this.add(searchButton);
    }

    private void fillFilterFromUrl() {
        Map<String, String> map = AppUtils.getMapByParams(params);
        if (map.containsKey("text")) {
            text.setValue(map.get("text"));
        }
        if (map.containsKey("createdFrom")) {
            createdFrom.setValue(LocalDate.parse(map.get("createdFrom")));
        }
        if (map.containsKey("createdTo")) {
            createdTo.setValue(LocalDate.parse(map.get("createdTo")));
        }
        if (map.containsKey("createdBy")) {
            for (UserDto user : userList) {
                if (user.getId() != null && String.valueOf(user.getId()).equals(map.get("createdBy"))) {
                    createdBy.setValue(user);
                }
            }
        }
        if (map.containsKey("noteType")) {
            for (NoteTypeEnum noteTypeEnum : noteTypeEnumList) {
                if (noteTypeEnum.getObjectName().equals(map.get("noteType"))) {
                    objectNameComboBox.setValue(noteTypeEnum);
                }
            }
        }
        if (map.containsKey("noteType") && map.containsKey("objectId")
                && !Transl.get(TextValues.SHOW_ALL_TEXT_VALUE).equals(map.get("noteType"))) {
            for (NoteObjectIdNameDto item : getIdComboBoxValues(map.get("noteType"))) {
                if (item.getId() != null && item.getId().equals(map.get("objectId"))) {
                    objectIdComboBox.setValue(item);
                }
            }
        }
        if (map.containsKey("showArchived")) {
            showArchived.setValue("true".equalsIgnoreCase(map.get("showArchived")));
        }
    }

    public void fillUrl() {
        String paramUrl = NoteView.ROUTE.concat("/");
        if (text.getValue() != null && !text.getValue().isEmpty()) {
            paramUrl = paramUrl.concat("&text=").concat(text.getValue());
        }
        if (createdFrom.getValue() != null) {
            paramUrl = paramUrl.concat("&createdFrom=").concat(createdFrom.getValue().toString());
        }
        if (createdTo.getValue() != null) {
            paramUrl = paramUrl.concat("&createdTo=").concat(createdTo.getValue().toString());
        }
        if (createdBy.getValue() != null && createdBy.getValue().getId() != null) {
            paramUrl = paramUrl.concat("&createdBy=").concat(String.valueOf(createdBy.getValue().getId()));
        }
        if (objectNameComboBox.getValue() != null) {
            paramUrl = paramUrl.concat("&noteType=").concat(objectNameComboBox.getValue().getObjectName());
        }
        if (objectIdComboBox.getValue() != null && objectIdComboBox.getValue().getId() != null) {
            paramUrl = paramUrl.concat("&objectId=").concat(objectIdComboBox.getValue().getId());
        }
        if (showArchived.getValue() != null) {
            paramUrl = paramUrl.concat("&showArchived=").concat(showArchived.getValue().toString());
        }

        UI.getCurrent().getPage().getHistory().pushState(null, paramUrl);
        historyBreadcrumbs.refreshLastBreadcrumb(paramUrl);
    }

    public NoteFilterDto getFilter() {
        NoteFilterDto filterDto = new NoteFilterDto();
        filterDto.setText(text.getValue());
        if (createdFrom.getValue() != null) {
            filterDto.setCreatedFrom(createdFrom.getValue().atStartOfDay());
        }
        if (createdTo.getValue() != null) {
            filterDto.setCreatedTo(createdTo.getValue().plusDays(1).atStartOfDay());
        }
        if (createdBy.getValue() != null && createdBy.getValue().getId() != null) {
            filterDto.setCreatedBy(createdBy.getValue());
        }
        if (objectNameComboBox.getValue() != null &&
                !TextValues.SHOW_ALL_TEXT_VALUE.equals(objectNameComboBox.getValue().getObjectName())) {
            filterDto.setNoteTypeEnum(objectNameComboBox.getValue());
        } else {
            filterDto.setNoteTypeEnum(NoteTypeEnum.ANY);
        }
        if (objectIdComboBox.getValue() != null && objectIdComboBox.getValue().getId() != null) {
            filterDto.setEntityId(objectIdComboBox.getValue().getId());
        }
        if (showArchived.getValue() != null) {
            filterDto.setShowArchived(showArchived.getValue());
        }
        if (noteType != null) {
            filterDto.setNoteTypeEnum(noteType);
        }
        filterDto.setPermission(permissionSet);
        return filterDto;
    }

    private String translateNoteTypeEnum(NoteTypeEnum value) {
        return Transl.get(value.getObjectName());
    }

    private void initiateEmptyIdComboBox() {
        List<NoteObjectIdNameDto> emptyList = new ArrayList<>();
        NoteObjectIdNameDto noteObjectIdNameDto = new NoteObjectIdNameDto();
        noteObjectIdNameDto.setName(Transl.get(TextValues.SHOW_ALL_TEXT_VALUE));
        emptyList.add(noteObjectIdNameDto);
        objectIdComboBox.setItems(emptyList);
        objectIdComboBox.setValue(emptyList.get(0));
        objectIdComboBox.setItemLabelGenerator(NoteObjectIdNameDto::getName);
    }

    private void changeIdComboBox(String objectName) {
        List<NoteObjectIdNameDto> noteObjectIdNameDtoList = getIdComboBoxValues(objectName);
        objectIdComboBox.setItems(noteObjectIdNameDtoList);
        objectIdComboBox.setValue(noteObjectIdNameDtoList.get(0));
        objectIdComboBox.setItemLabelGenerator(NoteObjectIdNameDto::getName);
    }

    private List<NoteObjectIdNameDto> getIdComboBoxValues(String objectName) {
        List<NoteObjectIdNameDto> noteObjectIdNameDtoList =
                noteComponentOperation.getNoteObjectIdNameDtoList(objectName);
        NoteObjectIdNameDto noteObjectIdNameDto = new NoteObjectIdNameDto();
        noteObjectIdNameDto.setName(Transl.get(TextValues.SHOW_ALL_TEXT_VALUE));
        noteObjectIdNameDtoList.add(0, noteObjectIdNameDto);
        return noteObjectIdNameDtoList;
    }
}
