import UIKit

class TopBarButton: UIView {
    let label = UILabel()
    let button = UIButton()
    let icon = UIImageView()
    var index: Int = 0
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        commonInit()
    }
    
    required init(coder: NSCoder) {
        super.init(coder: coder)!
        commonInit()
    }
    
    private func commonInit() {
        addSubview(button)
        addSubview(label)
        addSubview(icon)
        label.font = label.font.withSize(13)    /**Шрифт текста*/
        label.textAlignment = .center           /**Текст выравнивается по центру*/
    }
    
    override func layoutSubviews() {
        super.layoutSubviews()
        label.frame = self.bounds
        label.frame.origin.y = self.bounds.origin.y + 30
        button.frame = self.bounds
        icon.frame = self.bounds
        widthAnchor.constraint(equalToConstant: 50).isActive = true
        heightAnchor.constraint(equalToConstant: 50).isActive = true
        
    }
    
    public func updateLabel(text: String) {
        label.text = text
        setNeedsLayout()
    }
    
    public func updateIcon(image: UIImage) {
        icon.image = image
        setNeedsLayout()
    }

    public func updateIndex(index: Int) {
        self.index = index
    }
}
