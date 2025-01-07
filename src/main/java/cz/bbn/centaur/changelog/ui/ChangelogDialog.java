package cz.bbn.cerberus.changelog.ui;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.changelog.ChangelogService;
import cz.bbn.cerberus.changelog.dto.ChangelogVersionDto;
import cz.bbn.cerberus.changelog.ui.component.ChangelogDialogComponent;
import cz.bbn.cerberus.commons.VaadinValues;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

@Slf4j
public class ChangelogDialog extends AppDialog {

    private final ChangelogService changelogService;

    public ChangelogDialog(ChangelogService changelogService) {
        this.changelogService = changelogService;
        init();
    }

    void init() {
        setMinWidth(VaadinValues.DIALOG_MIN_WIDTH);
        setWidth("60em");
        setHeight("90%");
        setTitle(Transl.get("Changelog"));
        List<ChangelogVersionDto> changelogVersionDtoList = changelogService.findAllVersionList();
        ChangelogDialogComponent changelogDialogView =
                new ChangelogDialogComponent(changelogVersionDtoList, changelogService::getChangelog);
        changelogDialogView.init();
        setContent(changelogDialogView);

        Button cancelButton = VaadinComponents.getCloseButton();
        cancelButton.addClickListener(buttonClickEvent ->
                this.close());
        addButtons(cancelButton);
    }
}
