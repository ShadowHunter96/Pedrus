package cz.bbn.cerberus.changelog.ui.component;

import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextArea;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.changelog.dto.ChangelogVersionDto;
import cz.bbn.cerberus.changelog.persistance.ChangelogEntity;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.component.ui.interfaces.GetItemAction;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class ChangelogDialogComponent extends VerticalLayout {

    private final List<ChangelogVersionDto> changelogVersionDtoList;
    private final GetItemAction<ChangelogEntity> changelogDtoGetItemAction;

    public ChangelogDialogComponent(List<ChangelogVersionDto> changelogVersionDtoList,
                                    GetItemAction<ChangelogEntity> changelogDtoGetItemAction) {
        this.changelogVersionDtoList = changelogVersionDtoList;
        this.changelogDtoGetItemAction = changelogDtoGetItemAction;
    }

    public void init() {

        FormLayout formLayout = new FormLayout();
        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
        formLayout.setWidthFull();
        setSizeFull();

        ComboBox<ChangelogVersionDto> versionDtoComboBox = new ComboBox<>(Transl.get("Version"));
        TextField releaseDateTextField = new TextField(Transl.get("Release date"));
        releaseDateTextField.setReadOnly(true);

        TextArea changesTextArea = new TextArea(Transl.get("Changes"));
        changesTextArea.addClassName("area-dynamic-height");
        changesTextArea.setWidthFull();
        changesTextArea.setReadOnly(true);

        formLayout.add(versionDtoComboBox, releaseDateTextField);

        versionDtoComboBox.setItems(changelogVersionDtoList);
        versionDtoComboBox.setItemLabelGenerator(ChangelogVersionDto::getVersion);
        versionDtoComboBox.addValueChangeListener(event -> {
            ChangelogEntity changelogDto = changelogDtoGetItemAction.getItem(event.getValue().getVersion());
            releaseDateTextField.setValue(AppUtils.formatDateTime(changelogDto.getReleaseDate(), false));
            changesTextArea.setValue(changelogDto.getText());
        });

        if (!changelogVersionDtoList.isEmpty()) {
            ChangelogVersionDto changelogVersionDto = changelogVersionDtoList.get(0);
            ChangelogEntity changelogDto = changelogDtoGetItemAction.getItem(changelogVersionDto.getVersion());
            releaseDateTextField.setValue(AppUtils.formatDateTime(changelogDto.getReleaseDate(), false));
            changesTextArea.setValue(changelogDto.getText());
            versionDtoComboBox.setValue(changelogVersionDto);
        }
        this.add(formLayout, changesTextArea);
    }
}
