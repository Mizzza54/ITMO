import UIKit

class TopBar: UIStackView {

    override init(frame: CGRect) {
        super.init(frame: frame)
        commonInit()
    }
    
    required init(coder: NSCoder) {
        super.init(coder: coder)
        commonInit()
    }
    
    private func commonInit() {
        backgroundColor = .systemTeal
        axis = .horizontal
        distribution = .fill
        alignment = .fill
        isLayoutMarginsRelativeArrangement = false
        translatesAutoresizingMaskIntoConstraints = false
        layer.cornerRadius = 5
        clipsToBounds = true
    }
    
    override func layoutSubviews() {
        super.layoutSubviews()
    }
}
