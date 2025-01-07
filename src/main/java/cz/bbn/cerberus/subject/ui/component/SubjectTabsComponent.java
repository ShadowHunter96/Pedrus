package cz.bbn.cerberus.subject.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.tab.TabEntry;
import cz.bbn.cerberus.commons.component.ui.tab.TabsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

public class SubjectTabsComponent extends TabsComponent<SubjectDto> {

    public SubjectTabsComponent(String title, List<TabEntry> tabEntryList,
                                Button linkContactPersonButton, Button linkLabelButton,
                                Button changeSubjectTypeButton, boolean hasCustomPermission,
                                Button addDocument, Button icoButton,
                                EntityNewComponentOperation entityNewComponentOperation,
                                SubjectDto subjectDto) {
        super(title, tabEntryList, entityNewComponentOperation);

        if (SecurityUtils.hasPermission(Permission.SUBJECT_EDIT)
                && SecurityUtils.hasPermission(Permission.DOCUMENT_EDIT) && hasCustomPermission) {
            addToFooter(addDocument);
        }

        if (SecurityUtils.hasPermission(Permission.SUBJECT_EDIT)
                && SecurityUtils.hasCustomPermission(DomainEnum.SUBJECT_DOMAIN_NAME.getValue(),
                subjectDto.getId(), Permission.SUBJECT_LINK_CONTACT_PERSON.name())) {
            addToFooter(linkContactPersonButton);
        }

        if (SecurityUtils.hasPermission(Permission.SUBJECT_EDIT)
                && SecurityUtils.hasPermission(Permission.LABEL_SUBJECT_EDIT) && hasCustomPermission) {
            addToFooter(linkLabelButton);
        }

        if(Boolean.TRUE.equals(subjectDto.getLocalSubject())){
            addToButtonFooter(icoButton);
        }

        if (SecurityUtils.hasCustomPermission(DomainEnum.SUBJECT_DOMAIN_NAME.getValue(), subjectDto.getId(),
                        Permission.SUBJECT_CHANGE_SUBJECT_TYPE.name())) {
            addToFooter(changeSubjectTypeButton);
            changeTab(getSelectedTab());
        }

        addBackButton();
        if (hasCustomPermission) {
            addSaveButton();
        }
    }

}
